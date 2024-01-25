using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Zomp.SyncMethodGenerator;

/// <summary>
/// Rewrites a method synchronously.
/// </summary>
/// <remarks>
/// Creates a new instance of <see cref="AsyncToSyncRewriter"/>.
/// </remarks>
/// <param name="semanticModel">The semantic model.</param>
internal sealed class AsyncToSyncRewriter(SemanticModel semanticModel) : CSharpSyntaxRewriter
{
    public const string SyncOnly = "SYNC_ONLY";
    private const string SystemObject = "object";
    private const string SystemFunc = "System.Func";
    private const string ReadOnlyMemory = "System.ReadOnlyMemory";
    private const string Memory = "System.Memory";
    private const string TaskType = "System.Threading.Tasks.Task";
    private const string ValueTaskType = "System.Threading.Tasks.ValueTask";
    private const string IProgressInterface = "System.IProgress";
    private const string IAsyncResultInterface = "System.IAsyncResult";
    private const string CancellationTokenType = "System.Threading.CancellationToken";
    private const string ConfiguredTaskAwaitable = "System.Runtime.CompilerServices.ConfiguredTaskAwaitable";
    private const string ConfiguredValueTaskAwaitable = "System.Runtime.CompilerServices.ConfiguredValueTaskAwaitable";
    private const string ConfiguredCancelableAsyncEnumerable = "System.Runtime.CompilerServices.ConfiguredCancelableAsyncEnumerable";
    private const string IAsyncEnumerator = "System.Collections.Generic.IAsyncEnumerator";
    private const string FromResult = "FromResult";
    private const string Delay = "Delay";
    private static readonly HashSet<string> Drops = [IProgressInterface, CancellationTokenType];
    private static readonly HashSet<string> InterfacesToDrop = [IProgressInterface, IAsyncResultInterface];
    private static readonly Dictionary<string, string?> Replacements = new()
    {
        { "System.Collections.Generic.IAsyncEnumerable", "System.Collections.Generic.IEnumerable" },
        { IAsyncEnumerator, "System.Collections.Generic.IEnumerator" },
        { TaskType, null },
        { ValueTaskType, null },
        { ReadOnlyMemory, "System.ReadOnlySpan" },
        { Memory, "System.Span" },
    };

    private static readonly SymbolDisplayFormat GlobalDisplayFormat = new(
        globalNamespaceStyle: SymbolDisplayGlobalNamespaceStyle.Included,
        typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces,
        genericsOptions: SymbolDisplayGenericsOptions.None,
        miscellaneousOptions:
            SymbolDisplayMiscellaneousOptions.EscapeKeywordIdentifiers |
            SymbolDisplayMiscellaneousOptions.UseSpecialTypes);

    private static readonly SymbolDisplayFormat GlobalDisplayFormatWithTypeParameters = new(
        globalNamespaceStyle: SymbolDisplayGlobalNamespaceStyle.Included,
        typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces,
        genericsOptions: SymbolDisplayGenericsOptions.IncludeTypeParameters,
        miscellaneousOptions:
            SymbolDisplayMiscellaneousOptions.EscapeKeywordIdentifiers |
            SymbolDisplayMiscellaneousOptions.UseSpecialTypes);

    private static readonly SymbolDisplayFormat NamespaceDisplayFormat = new(
        typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces,
        genericsOptions: SymbolDisplayGenericsOptions.None,
        miscellaneousOptions:
            SymbolDisplayMiscellaneousOptions.EscapeKeywordIdentifiers |
            SymbolDisplayMiscellaneousOptions.UseSpecialTypes);

    private readonly SemanticModel semanticModel = semanticModel;
    private readonly HashSet<IParameterSymbol> removedParameters = [];
    private readonly Dictionary<string, string> renamedLocalFunctions = [];
    private readonly ImmutableArray<ReportedDiagnostic>.Builder diagnostics = ImmutableArray.CreateBuilder<ReportedDiagnostic>();

    private enum SyncOnlyDirectiveType
    {
        None,
        SyncOnly,
        NotSyncOnly,
        Invalid,
    }

    private enum SpecialMethod
    {
        None,
        FromResult,
        Delay,
    }

    /// <summary>
    /// Gets the diagnostics messages.
    /// </summary>
    public ImmutableArray<ReportedDiagnostic> Diagnostics => diagnostics.ToImmutable();

    /// <inheritdoc/>
    public override SyntaxNode? VisitConditionalAccessExpression(ConditionalAccessExpressionSyntax node)
    {
        var chain = new List<ExtensionExprSymbol>();
        ConditionalAccessExpressionSyntax? curNode = node;

        ExtensionExprSymbol? GetExtensionExprSymbol(InvocationExpressionSyntax invocation)
            => GetSymbol(invocation) is not IMethodSymbol
            { IsExtensionMethod: true, ReducedFrom: { } reducedFrom, ReturnType: { } returnType }
            ? null : new(invocation, reducedFrom, returnType);

        while (curNode is { WhenNotNull: { } whenNotNull })
        {
            var ext = whenNotNull switch
            {
                InvocationExpressionSyntax ies => GetExtensionExprSymbol(ies),
                ConditionalAccessExpressionSyntax { Expression: InvocationExpressionSyntax ies } caes => GetExtensionExprSymbol(ies),
                _ => null,
            };

            if (ext is not null)
            {
                chain.Add(ext);
            }

            curNode = whenNotNull as ConditionalAccessExpressionSyntax;
        }

        if (chain.Count == 0)
        {
            return base.VisitConditionalAccessExpression(node)!;
        }

        var leftOfTheDot = (ExpressionSyntax)Visit(node.Expression)!;

        static BinaryExpressionSyntax CheckNull(ExpressionSyntax expr) => BinaryExpression(
            SyntaxKind.EqualsExpression,
            CastExpression(NullableType(IdentifierName(SystemObject)), expr).AppendSpace(),
            LiteralExpression(SyntaxKind.NullLiteralExpression).PrependSpace());

        var argumentType = GetSymbol(leftOfTheDot) ?? throw new InvalidOperationException("Can't process");
        var funcArgumentType = GetReturnType(argumentType);

        IdentifierNameSyntax toCheckForNullExpr;
        GenericNameSyntax? funcExpr = null;

        var statements = new List<StatementSyntax>();
        var parameter = Identifier("param");
        if (leftOfTheDot is IdentifierNameSyntax ins && chain.Count == 1)
        {
            toCheckForNullExpr = ins;
        }
        else
        {
            var argumentTypeExpr = ProcessSymbol(funcArgumentType);

            var separated = SeparatedList([(TypeSyntax)NullableType(argumentTypeExpr), NullableType(ProcessSymbol(chain[^1].ReturnType))]);

            var type = TypeArgumentList(separated);
            funcExpr = GenericName(
                Identifier(Global(SystemFunc)),
                type);

            toCheckForNullExpr = IdentifierName(parameter);
        }

        ExpressionSyntax lastExpression = null!;

        for (var i = 0; i < chain.Count; i++)
        {
            var (callSymbol, reducedSymbol, returnType) = chain[i];
            var unwrappedExpr = UnwrapExtension(callSymbol, /*Fixme*/ false, reducedSymbol, toCheckForNullExpr);

            var condition = CheckNull(toCheckForNullExpr);

            if (i == chain.Count - 1)
            {
                var conditional = ConditionalExpression(
                    condition,
                    CastExpression(NullableType(ProcessSymbol(returnType)), LiteralExpression(SyntaxKind.NullLiteralExpression)),
                    unwrappedExpr.PrependSpace());

                lastExpression = conditional;

                statements.Add(ReturnStatement(conditional.PrependSpace()));
                continue;
            }

            var returnNullStatement = ReturnStatement(LiteralExpression(SyntaxKind.NullLiteralExpression).PrependSpace());

            var ifBlock = ((ICollection<StatementSyntax>)[returnNullStatement]).CreateBlock(3);
            statements.Add(IfStatement(CheckNull(toCheckForNullExpr), ifBlock));

            var toCheckForNull = Identifier($"check{i}");
            var localType = ProcessSymbol(returnType); // reduced will return generic

            var declarator = VariableDeclarator(toCheckForNull.AppendSpace(), null, EqualsValueClause(unwrappedExpr));
            var declaration = VariableDeclaration(localType.AppendSpace(), SeparatedList([declarator]));
            var intermediaryNullCheck = LocalDeclarationStatement(declaration);

            statements.Add(intermediaryNullCheck);

            toCheckForNullExpr = IdentifierName(toCheckForNull);
        }

        if (funcExpr is null)
        {
            return ParenthesizedExpression(lastExpression);
        }

        var parameterList = ParameterList(SeparatedList([Parameter(parameter)]));
        var lambda = chain.Count == 1
            ? ParenthesizedLambdaExpression(parameterList, lastExpression)
            : ParenthesizedLambdaExpression(parameterList, statements.CreateBlock(2));

        var arguments = SeparatedList([Argument(leftOfTheDot)]);
        return InvocationExpression(ParenthesizedExpression(CastExpression(funcExpr, ParenthesizedExpression(lambda))), ArgumentList(arguments));
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitNullableType(NullableTypeSyntax node)
    {
        var @base = (NullableTypeSyntax)base.VisitNullableType(node)!;
        return @base.WithElementType(ProcessType(@base.ElementType)).WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitGenericName(GenericNameSyntax node)
    {
        var @base = (GenericNameSyntax)base.VisitGenericName(node)!;

        var symbol = GetSymbol(node);

        if (symbol is not INamedTypeSymbol)
        {
            return @base;
        }

        var genericName = GetNameWithoutTypeParams(symbol);

        string? newType = null;
        if (Replacements.TryGetValue(genericName, out var replacement))
        {
            if (replacement is not null)
            {
                newType = Global(replacement);
            }
        }
        else
        {
            newType = symbol.ToDisplayString(GlobalDisplayFormat);
        }

        if (newType is not null)
        {
            return @base.WithIdentifier(SyntaxFactory.Identifier(newType))
                .WithTriviaFrom(@base);
        }
        else
        {
            return @base.TypeArgumentList.Arguments[0].WithTriviaFrom(@base);
        }
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitIdentifierName(IdentifierNameSyntax node)
    {
        var @base = (IdentifierNameSyntax)base.VisitIdentifierName(node)!;
        if (renamedLocalFunctions.TryGetValue(@base.Identifier.ValueText, out var newName))
        {
            return @base.WithIdentifier(SyntaxFactory.Identifier(newName));
        }
        else if (node.Parent is not MemberAccessExpressionSyntax
            && GetSymbol(node) is IFieldSymbol { IsStatic: true } fieldSymbol)
        {
            var typeString = fieldSymbol.ContainingType.ToDisplayString(GlobalDisplayFormatWithTypeParameters);
            return @base.WithIdentifier(Identifier($"{typeString}.{fieldSymbol.Name}"));
        }

        return @base;
    }

    public override SyntaxNode? VisitLocalFunctionStatement(LocalFunctionStatementSyntax node)
    {
        var @base = (LocalFunctionStatementSyntax)base.VisitLocalFunctionStatement(node)!;

        if (semanticModel.GetTypeInfo(node.ReturnType).Type is not INamedTypeSymbol symbol)
        {
            return @base;
        }

        var newNode = @base;

        var identifier = @base.Identifier;
        if (identifier.ValueText.EndsWithAsync())
        {
            var newName = RemoveAsync(identifier.ValueText);
            renamedLocalFunctions.Add(identifier.ValueText, newName);
            newNode = @base.WithIdentifier(SyntaxFactory.Identifier(newName));
        }

        var nonEmptyAttributes = SyntaxFactory.List(@base.AttributeLists.Where(z => z.Attributes.Any()));

        return newNode
            .WithReturnType(GetReturnType(@base.ReturnType, symbol))
            .WithModifiers(StripAsyncModifier(@base.Modifiers))
            .WithAttributeLists(nonEmptyAttributes)
            .WithTriviaFrom(@base);
    }

    public override SyntaxNode? VisitParameterList(ParameterListSyntax node)
    {
        var newNode = (ParameterListSyntax)base.VisitParameterList(node)!;
        bool IsValidParameter(ParameterSyntax ps)
        {
            if (semanticModel.GetDeclaredSymbol(ps) is not { } symbol)
            {
                return true;
            }

            var nts = symbol.Type;

            var name = GetNameWithoutTypeParams(nts);
            if (ShouldRemoveType(nts))
            {
                removedParameters.Add(symbol);
                return false;
            }

            return true;
        }

        var invalid = node.Parameters.GetIndices((v, _) => !IsValidParameter(v));
        var newParams = RemoveAtRange(newNode.Parameters, invalid);
        return newNode.WithParameters(newParams);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitParameter(ParameterSyntax node)
    {
        var @base = (ParameterSyntax)base.VisitParameter(node)!;
        if (node.Type is null or NullableTypeSyntax or GenericNameSyntax)
        {
            return @base;
        }

        return @base.WithType(ProcessType(node.Type)).WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
    {
        var @base = (MemberAccessExpressionSyntax)base.VisitMemberAccessExpression(node)!;

        var exprSymbol = GetSymbol(node.Expression);
        if (exprSymbol is ITypeSymbol && node.Expression is TypeSyntax type)
        {
            // Rewrite static invocation (eg. File.ReadAllTextAsync)
            var newType = ProcessType(type);
            if (newType != type)
            {
                @base = @base.WithExpression(newType);
            }
        }

        if (@base.Name.Identifier.ValueText == "Span"
            && GetSymbol(node.Expression) is { } symbol
            && GetNameWithoutTypeParams(GetReturnType(symbol)) is Memory or ReadOnlyMemory)
        {
            return @base.Expression;
        }
        else if (@base.Name.Identifier.ValueText.EndsWithAsync())
        {
            return @base.WithName(@base.ChangeIdentifier(RemoveAsync(@base.Name.Identifier.ValueText)));
        }

        return @base;
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitInvocationExpression(InvocationExpressionSyntax node)
    {
        var @base = (InvocationExpressionSyntax)base.VisitInvocationExpression(node)!;

        string? newName = null;
        var symbol = GetSymbol(node);

        if (symbol is null)
        {
            return @base;
        }

        if (symbol is not IMethodSymbol methodSymbol)
        {
            throw new InvalidOperationException($"Could not get symbol of {node}");
        }

        if (@base.Expression is IdentifierNameSyntax ins && ins.Identifier.ValueText.EndsWithAsync())
        {
            newName = RemoveAsync(ins.Identifier.ValueText);

            /*
            var siblings = methodSymbol.ContainingType.GetMembers().Where(z => z is IMethodSymbol).ToList();
            var hasSync = siblings.Any(z => z.Name == newName);
            var hasAsyncWithAttr = siblings.Any(z => z.Name == ins.Identifier.ValueText
                && z.GetAttributes().Any(z
                    => z.AttributeClass is not null && IsCreateSyncVersionAttribute(z.AttributeClass)));
            */

            if (string.IsNullOrWhiteSpace(newName))
            {
                // Should Return diagnostics message
                return @base;
            }

            return @base.WithExpression(SyntaxFactory.IdentifierName(newName));
        }
        else if (@base.Expression is GenericNameSyntax gn && gn.Identifier.Text.EndsWithAsync())
        {
            newName = RemoveAsync(gn.Identifier.Text);
            return @base.WithExpression(gn.WithIdentifier(SyntaxFactory.Identifier(newName)));
        }

        var returnType = GetNameWithoutTypeParams(methodSymbol.ReturnType);
        var isMemory = returnType is ReadOnlyMemory or Memory;

        if (isMemory)
        {
            newName = GetNewName(methodSymbol);
        }

        if (@base.Expression is not MemberAccessExpressionSyntax { } memberAccess)
        {
            return @base;
        }

        if (IsTaskExtension(methodSymbol))
        {
            return memberAccess.Expression;
        }

        if (methodSymbol is { IsExtensionMethod: true, ReducedFrom: { } reducedFrom })
        {
            return UnwrapExtension(@base, isMemory, reducedFrom, memberAccess.Expression);
        }

        if (memberAccess.Name is not SimpleNameSyntax { Identifier.ValueText: { } name })
        {
            return @base;
        }

        if (name.EndsWithAsync() || returnType == IAsyncEnumerator)
        {
            newName = RemoveAsync(name);
        }
        else
        {
            var specialMethod = IsSpecialMethod(methodSymbol);
            if (specialMethod == SpecialMethod.FromResult && @base.ArgumentList.Arguments is [var singleArg])
            {
                return singleArg.Expression;
            }
            else if (specialMethod == SpecialMethod.Delay)
            {
                return InvocationExpression(MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, IdentifierName(Global("System.Threading.Thread")), IdentifierName("Sleep")), @base.ArgumentList).WithTriviaFrom(@base);
            }
        }

        if (newName == null)
        {
            return @base;
        }

        return @base.WithExpression(memberAccess.WithName(SyntaxFactory.IdentifierName(newName)));
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitAwaitExpression(AwaitExpressionSyntax node)
    {
        var @base = (AwaitExpressionSyntax)base.VisitAwaitExpression(node)!;

        return @base.Expression.WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitParenthesizedExpression(ParenthesizedExpressionSyntax node)
    {
        var dropParentheses = node.Expression is AwaitExpressionSyntax && node.Parent is not InterpolationSyntax;
        var @base = (ParenthesizedExpressionSyntax)base.VisitParenthesizedExpression(node)!;
        return dropParentheses ? @base.Expression.WithTriviaFrom(@base) : @base;
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitArrayType(ArrayTypeSyntax node)
    {
        var @base = (ArrayTypeSyntax)base.VisitArrayType(node)!;
        return @base.WithElementType(ProcessType(@base.ElementType)).WithTriviaFrom(@base);
    }

    public override SyntaxNode? VisitInterpolation(InterpolationSyntax node)
    {
        var @base = (InterpolationSyntax)base.VisitInterpolation(node)!;
        if (@base.Expression is not ParenthesizedExpressionSyntax)
        {
            var newExpression = SyntaxFactory.ParenthesizedExpression(@base.Expression);
            @base = @base.WithExpression(newExpression);
        }

        return @base;
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitObjectCreationExpression(ObjectCreationExpressionSyntax node)
    {
        var @base = (ObjectCreationExpressionSyntax)base.VisitObjectCreationExpression(node)!;
        var symbol = GetSymbol(node);
        if (symbol is null
            or { ContainingType.IsGenericType: true }
            or INamedTypeSymbol { IsGenericType: true })
        {
            return @base;
        }

        var newType = ProcessSymbol(symbol.ContainingType);
        return newType == node.Type ? @base : @base.WithType(newType);
    }

    public override SyntaxNode? VisitIfStatement(IfStatementSyntax node)
    {
        var substituteIfWithEmpty = node.Statement is ExpressionStatementSyntax es && ShouldRemoveArgument(es.Expression);
        var substituteElseWithEmpty = node.Else is ElseClauseSyntax { Statement: ExpressionStatementSyntax { Expression: { } e } } && ShouldRemoveArgument(e);

        var retVal = (IfStatementSyntax)base.VisitIfStatement(node)!;

        if (substituteIfWithEmpty)
        {
            retVal = retVal.WithStatement(SyntaxFactory.Block());
        }

        if (substituteElseWithEmpty)
        {
            retVal = retVal.WithElse(SyntaxFactory.ElseClause(SyntaxFactory.Block()));
        }

        return retVal;
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitSwitchSection(SwitchSectionSyntax node)
    {
        var statementProcessor = new StatementProcessor(this, node.Statements);
        if (statementProcessor.HasErrors)
        {
            return node;
        }

        var @base = (SwitchSectionSyntax)base.VisitSwitchSection(node)!;
        var newStatements = statementProcessor.PostProcess(@base.Statements);
        var retVal = @base.WithStatements(newStatements);

        return retVal;
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitBlock(BlockSyntax node)
    {
        var statementProcessor = new StatementProcessor(this, node.Statements);
        if (statementProcessor.HasErrors)
        {
            return node;
        }

        var @base = (BlockSyntax)base.VisitBlock(node)!;

        var newStatements = statementProcessor.PostProcess(@base.Statements);
        var retVal = @base.WithStatements(newStatements);

        var lastToken = retVal.CloseBraceToken;
        if (ProcessTrivia(node.CloseBraceToken.LeadingTrivia, statementProcessor.DirectiveStack) is var (_, newStatements2, newTrivia))
        {
            var oldStatements = retVal.Statements.ToList();
            oldStatements.AddRange(newStatements2.ToList());
            retVal = retVal.WithStatements(SyntaxFactory.List(oldStatements)).WithCloseBraceToken(lastToken.WithLeadingTrivia(newTrivia));
        }

        return retVal;
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitTypeArgumentList(TypeArgumentListSyntax node)
    {
        // Ensure that Func<Task<T>> is not removed
        var isFunc = node.Parent is { } parent
            && semanticModel.GetTypeInfo(parent).Type is INamedTypeSymbol { IsGenericType: true } n
            && GetNameWithoutTypeParams(n) is SystemFunc;

        // Do not remove Task<T>, but remove Task inside a Func<>
        bool RemoveType(TypeSyntax z, int index) =>
            !(isFunc && index == node.Arguments.Count - 1 && z is GenericNameSyntax)
            && semanticModel.GetTypeInfo(z).Type is { } type
            && ShouldRemoveType(type);

        var indicesToRemove = node.Arguments.GetIndices(RemoveType);

        var @base = (TypeArgumentListSyntax)base.VisitTypeArgumentList(node)!;
        var newSep = RemoveSeparators(@base.Arguments.GetSeparators().ToList(), indicesToRemove);

        var newArguments = SyntaxFactory.SeparatedList(
            RemoveAtRange(@base.Arguments, indicesToRemove)
            .Select(z => z.SyntaxTree == node.SyntaxTree ? ProcessType(z) : z),
            newSep);
        return @base.WithArguments(newArguments);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitTypeConstraint(TypeConstraintSyntax node)
    {
        var @base = (TypeConstraintSyntax)base.VisitTypeConstraint(node)!;
        var newType = ProcessType(@base.Type);
        if (newType == @base.Type)
        {
            return @base;
        }

        return @base.WithType(newType).WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitCatchDeclaration(CatchDeclarationSyntax node)
    {
        var @base = (CatchDeclarationSyntax)base.VisitCatchDeclaration(node)!;
        return @base.WithType(ProcessType(node.Type)).WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitMethodDeclaration(MethodDeclarationSyntax node)
    {
        var @base = base.VisitMethodDeclaration(node) as MethodDeclarationSyntax ?? throw new InvalidOperationException("Can't cast");
        var returnType = node.ReturnType;

        if (semanticModel.GetTypeInfo(returnType).Type is not INamedTypeSymbol symbol)
        {
            return @base;
        }

        var genericReturnType = returnType as GenericNameSyntax;

        var isTask = genericReturnType is null && symbol.ToString() is TaskType or ValueTaskType;

        var hasAsync = @base.Modifiers.Any(z => z.IsKind(SyntaxKind.AsyncKeyword));

        if (!hasAsync && !IsTypeOfInterest(symbol))
        {
            return @base;
        }

        var modifiers = @base.Modifiers.Where(z => !z.IsKind(SyntaxKind.AsyncKeyword));

        var originalName = @base.Identifier.Text;
        var newName = RemoveAsync(originalName);

        // Documentation
        var trivia = node.GetLeadingTrivia();
        var newTriviaList = trivia;
        var comments = trivia.FirstOrDefault(t => t.IsKind(SyntaxKind.SingleLineDocumentationCommentTrivia));
        var xml = comments.GetStructure();

        DocumentationCommentTriviaSyntax? comment = null;
        if (xml is DocumentationCommentTriviaSyntax syntax)
        {
            comment = syntax;
            var i = 0;
            var indicesToRemove = new List<int>();

            foreach (var commentLine in comment.Content)
            {
                if (commentLine is XmlElementSyntax xes)
                {
                    var attribute = xes.StartTag.Attributes.FirstOrDefault(a => a is XmlNameAttributeSyntax) as XmlNameAttributeSyntax;
                    if (attribute is not null
                        && GetSymbol(attribute.Identifier) is IParameterSymbol paramSymbol
                        && removedParameters.Contains(paramSymbol))
                    {
                        indicesToRemove.Add(i - 1); // preceding slashes
                        indicesToRemove.Add(i);
                    }
                }

                ++i;
            }

            var newContent = RemoveAtRange(comment.Content, indicesToRemove);
            comment = comment.WithContent(newContent);
            var newTrivia = SyntaxFactory.Trivia(comment);
            newTriviaList = trivia.Replace(comments, newTrivia);
        }

        static bool Preprocessors(SyntaxTrivia st)
        {
            return st.IsKind(SyntaxKind.IfDirectiveTrivia)
                || st.IsKind(SyntaxKind.ElifDirectiveTrivia)
                || st.IsKind(SyntaxKind.ElseDirectiveTrivia)
                || st.IsKind(SyntaxKind.EndIfDirectiveTrivia)
                || st.IsKind(SyntaxKind.RegionDirectiveTrivia)
                || st.IsKind(SyntaxKind.EndRegionDirectiveTrivia)
                || st.IsKind(SyntaxKind.DisabledTextTrivia);
        }

        var z = newTriviaList.Where(Preprocessors).ToList();

        while (newTriviaList.FirstOrDefault(Preprocessors) is { } preprocessor
            && preprocessor != default)
        {
            newTriviaList = newTriviaList.Remove(preprocessor);
        }

        if (ShouldRemoveArrowExpression(node.ExpressionBody))
        {
            @base = @base
                .WithExpressionBody(null)
                .WithBody(CreateEmptyBody())
                .WithSemicolonToken(default);
        }

        var newReturnType = GetReturnType(@base.ReturnType, symbol);

        var nonEmptyAttributes = SyntaxFactory.List(@base.AttributeLists.Where(z => z.Attributes.Any()));

        var retVal = @base
            .WithIdentifier(SyntaxFactory.Identifier(newName))
            .WithReturnType(newReturnType)
            .WithModifiers(StripAsyncModifier(@base.Modifiers))
            .WithAttributeLists(nonEmptyAttributes)
            .WithLeadingTrivia(newTriviaList)
            ;

        return retVal;
    }

    public override SyntaxNode? VisitArgument(ArgumentSyntax node)
    {
        // Handles nameof(Type)
        var @base = (ArgumentSyntax)base.VisitArgument(node)!;
        if (GetSymbol(node.Expression) is ITypeSymbol { } typeSymbol)
        {
            return @base.WithExpression(ProcessSymbol(typeSymbol)).WithTriviaFrom(@base);
        }

        return @base;
    }

    public override SyntaxNode? VisitArgumentList(ArgumentListSyntax node)
    {
        ImmutableArray<IParameterSymbol>? nullableParameters = null;

        if (node.Parent is { } parent
            && GetSymbol(parent) is IMethodSymbol { Parameters: { Length: > 0 } @params })
        {
            nullableParameters = @params;
        }

        bool ShouldRemoveArgumentLocal(ArgumentSyntax arg, int index)
        {
            var byExpression = ShouldRemoveArgument(arg.Expression);
            if (byExpression || nullableParameters is not { } parameters)
            {
                return byExpression;
            }

            var isParams = parameters[^1].IsParams;

            var nameColon = arg.NameColon?.Name.Identifier.ValueText;

            var param = nameColon is not null ? parameters.Single(z => z.Name == nameColon)
                : index < parameters.Length ? parameters[index] : null;

            return param is not null && ShouldRemoveType(param.Type);
        }

        var @base = (ArgumentListSyntax)base.VisitArgumentList(node)!;
        var invalid = node.Arguments.GetIndices(ShouldRemoveArgumentLocal);

        var newList = RemoveAtRange(@base.Arguments, invalid);
        return @base.WithArguments(newList);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitForEachStatement(ForEachStatementSyntax node)
    {
        var @base = (ForEachStatementSyntax)base.VisitForEachStatement(node)!;
        return @base.WithAwaitKeyword(default).WithType(ProcessType(node.Type)).WithTriviaFrom(@base);
    }

    public override SyntaxNode? VisitParenthesizedLambdaExpression(ParenthesizedLambdaExpressionSyntax node)
    {
        var @base = (ParenthesizedLambdaExpressionSyntax)base.VisitParenthesizedLambdaExpression(node)!;
        if (node.ExpressionBody is { } expr && ShouldRemoveArgument(expr))
        {
            @base = @base
                .WithExpressionBody(null)
                .WithBody(CreateEmptyBody());
        }

        return @base.WithModifiers(StripAsyncModifier(@base.Modifiers)).WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
    {
        var @base = (LocalDeclarationStatementSyntax)base.VisitLocalDeclarationStatement(node)!;

        var variableTypeName = node.Declaration.Type;

        var variableType = semanticModel
            .GetSymbolInfo(variableTypeName)
            .Symbol;

        TypeSyntax? newTypeSyntax = null;

        SeparatedSyntaxList<VariableDeclaratorSyntax>? separatedVariableNames = null;
        if (variableType is INamedTypeSymbol { IsGenericType: true } n
            && GetNameWithoutTypeParams(n) is SystemFunc)
        {
            var newVariableNames = new List<VariableDeclaratorSyntax>();
            foreach (var variable in @base.Declaration.Variables)
            {
                newVariableNames.Add(SyntaxFactory.VariableDeclarator(RemoveAsync(variable.Identifier.Text))
                    .WithTrailingTrivia(variable.Identifier.TrailingTrivia).WithInitializer(variable.Initializer));
            }

            var typeArgs = n.TypeArguments;

            if (typeArgs[^1].ToString() is TaskType or ValueTaskType
                && @base.Declaration.Type is GenericNameSyntax gns)
            {
                var newType = Global("System.Action");

                var list = new List<TypeSyntax>();
                if (typeArgs.Length > 1)
                {
                    for (var i = 0; i < typeArgs.Length - 1; i++)
                    {
                        list.Add(ProcessSymbol(typeArgs[i]));
                    }

                    var originalSeparators = gns.TypeArgumentList.Arguments.GetSeparators();

                    var separatedList = SyntaxFactory.SeparatedList(list, originalSeparators);
                    newTypeSyntax = SyntaxFactory.GenericName(SyntaxFactory.Identifier(newType), SyntaxFactory.TypeArgumentList(separatedList));
                }
                else
                {
                    newTypeSyntax = SyntaxFactory.IdentifierName(newType);
                }

                newTypeSyntax = newTypeSyntax.WithTriviaFrom(node.Declaration.Type);
            }

            separatedVariableNames = SyntaxFactory.SeparatedList(newVariableNames, node.Declaration.Variables.GetSeparators());
        }
        else
        {
            var invalid = node.Declaration.Variables.GetIndices(RemoveDeclarator);
            if (invalid.Length > 0)
            {
                separatedVariableNames = RemoveAtRange(@base.Declaration.Variables, invalid);
            }
        }

        var retval = @base.WithAwaitKeyword(default).WithTriviaFrom(@base);
        if (separatedVariableNames is not null)
        {
            retval = retval.WithDeclaration(retval.Declaration.WithVariables(separatedVariableNames.Value));
        }

        if (newTypeSyntax is not null)
        {
            retval = retval.WithDeclaration(retval.Declaration.WithType(newTypeSyntax));
        }

        return retval;
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitVariableDeclaration(VariableDeclarationSyntax node)
    {
        var @base = (VariableDeclarationSyntax)base.VisitVariableDeclaration(node)!;
        var type = node.Type;
        var newType = @base.Type;

        if (newType == type)
        {
            // not replaced
            newType = ProcessType(type);

            if (newType == type)
            {
                return @base;
            }
        }

        return @base.WithType(newType).WithTriviaFrom(@base);
    }

    public override SyntaxNode? VisitAttributeList(AttributeListSyntax node)
    {
        bool ShouldRemoveAttribute(AttributeSyntax attributeSyntax)
        {
            if (GetSymbol(attributeSyntax) is not IMethodSymbol attributeSymbol)
            {
                return true;
            }

            var attributeContainingTypeSymbol = attributeSymbol.ContainingType;

            // Is the attribute [CreateSyncVersion] attribute?
            return IsCreateSyncVersionAttribute(attributeContainingTypeSymbol);
        }

        var @base = (AttributeListSyntax)base.VisitAttributeList(node)!;
        var indices = node.Attributes.GetIndices(ShouldRemoveAttribute);
        var newList = RemoveAtRange(@base.Attributes, indices);
        return @base.WithAttributes(newList);
    }

    public override SyntaxNode? VisitAttribute(AttributeSyntax node)
    {
        var @base = (AttributeSyntax)base.VisitAttribute(node)!;

        if (GetSymbol(node.Name) is not IMethodSymbol ms)
        {
            return @base;
        }

        var retval = @base.WithName(ProcessSymbol(ms.ContainingType));
        return retval;
    }

    public override SyntaxNode? VisitCastExpression(CastExpressionSyntax node)
    {
        var @base = (CastExpressionSyntax)base.VisitCastExpression(node)!;
        return @base.WithType(ProcessType(node.Type)).WithTriviaFrom(@base);
    }

    public override SyntaxNode? VisitDeclarationPattern(DeclarationPatternSyntax node)
    {
        var @base = (DeclarationPatternSyntax)base.VisitDeclarationPattern(node)!;
        return @base.WithType(ProcessType(node.Type)).WithTriviaFrom(@base);
    }

    public override SyntaxNode? VisitTypeOfExpression(TypeOfExpressionSyntax node)
    {
        var @base = (TypeOfExpressionSyntax)base.VisitTypeOfExpression(node)!;
        return @base.WithType(ProcessType(node.Type)).WithTriviaFrom(@base);
    }

    public override SyntaxNode? VisitBinaryExpression(BinaryExpressionSyntax node)
    {
        var @base = (BinaryExpressionSyntax)base.VisitBinaryExpression(node)!;

        if (@base.OperatorToken.IsKind(SyntaxKind.IsKeyword) && GetSymbol(node.Right) is INamedTypeSymbol typeSymbol)
        {
            @base = @base.WithRight(ProcessSymbol(typeSymbol)).WithTriviaFrom(@base);
        }

        return @base;
    }

    internal static bool IsTypeOfInterest(ITypeSymbol symbol)
    {
        var genericName = GetNameWithoutTypeParams(symbol);
        return Replacements.ContainsKey(genericName);
    }

    private static string MakeType(ISymbol symbol)
        => symbol switch
        {
            INamedTypeSymbol => symbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat),
            _ => symbol.Name,
        };

    private static IdentifierNameSyntax ProcessSymbol(ITypeSymbol typeSymbol)
        => SyntaxFactory.IdentifierName(ProcessSymbolInternal(typeSymbol));

    private static string ProcessSymbolInternal(ITypeSymbol typeSymbol) => typeSymbol switch
    {
        INamedTypeSymbol nts => MakeType(nts),
        IArrayTypeSymbol ats => MakeType(ats.ElementType) + "[]",
        _ => typeSymbol.Name,
    };

    private static SpecialMethod IsSpecialMethod(IMethodSymbol methodSymbol)
    {
        string GetContainingType() => GetNameWithoutTypeParams(methodSymbol.ContainingType);
        return methodSymbol.Name switch
        {
            Delay when GetContainingType() == TaskType => SpecialMethod.Delay,
            FromResult when GetContainingType() is TaskType or ValueTaskType => SpecialMethod.FromResult,
            _ => SpecialMethod.None,
        };
    }

    private static SyntaxList<StatementSyntax> ProcessStatements(SyntaxList<StatementSyntax> list, Dictionary<int, ExtraNodeInfo> extraNodeInfoList)
    {
        var newStatements = new List<StatementSyntax>();

        for (var i = 0; i < list.Count; ++i)
        {
            var statement = list[i];

            if (!extraNodeInfoList.TryGetValue(i, out var extra))
            {
                newStatements.Add(statement);
                continue;
            }

            var (statementGetsDropped, statements, trivia) = extra;

            for (var j = 0; j < statements.Count; ++j)
            {
                var syncOnlyStatement = statements[j];
                var syncOnlyStatementWithTrivia = syncOnlyStatement;

                if (j == statements.Count - 1 && trivia.Count > 0)
                {
                    var trailingTrivia = syncOnlyStatement.GetTrailingTrivia().ToList();
                    trailingTrivia.AddRange(trivia.Where(t => !t.IsKind(SyntaxKind.WhitespaceTrivia)));
                    syncOnlyStatementWithTrivia = syncOnlyStatementWithTrivia.WithTrailingTrivia(trailingTrivia);
                }

                newStatements.Add(syncOnlyStatementWithTrivia);
            }

            if (!statementGetsDropped)
            {
                var newStatement = statement;
                if (extra is not null)
                {
                    newStatement = newStatement.WithLeadingTrivia(trivia);
                }

                newStatements.Add(newStatement);
            }
        }

        return SyntaxFactory.List(newStatements);
    }

    private static string GetNameWithoutTypeParams(ISymbol symbol)
        => symbol.ToDisplayString(NamespaceDisplayFormat);

    private static SyntaxTokenList StripAsyncModifier(SyntaxTokenList list)
        => SyntaxFactory.TokenList(list.Where(z => !z.IsKind(SyntaxKind.AsyncKeyword)));

    private static string RemoveAsync(string original)
        => Regex.Replace(original, "Async", string.Empty);

    private static bool CanDropIf(IfStatementSyntax ifStatement)
        => ifStatement.Statement is BlockSyntax { Statements.Count: 0 } or null
        && (ifStatement.Else is null || CanDropElse(ifStatement.Else))
        && ifStatement.Condition is BinaryExpressionSyntax or LiteralExpressionSyntax;

    private static bool CanDropSwitch(SwitchStatementSyntax switchStatement)
        => switchStatement.Sections.All(CanDropSwitchSection);

    private static bool CanDropSwitchSection(SwitchSectionSyntax section)
        => section.Statements is { Count: 0 } || section.Statements[0] is BreakStatementSyntax;

    private static bool CanDropElse(ElseClauseSyntax @else)
        => @else.Statement switch
        {
            BlockSyntax { Statements.Count: 0 } => true,
            IfStatementSyntax @if => CanDropIf(@if),
            _ => false,
        };

    private static bool IsCreateSyncVersionAttribute(INamedTypeSymbol s)
        => s.ToDisplayString() == SyncMethodSourceGenerator.QualifiedCreateSyncVersionAttribute;

    private static SyntaxList<TNode> RemoveAtRange<TNode>(SyntaxList<TNode> list, IEnumerable<int> indices)
        where TNode : SyntaxNode
    {
        var newContent = list;

        foreach (var z in indices.OrderByDescending(z => z))
        {
            newContent = newContent.RemoveAt(z);
        }

        return newContent;
    }

    private static List<SyntaxToken> RemoveSeparators(List<SyntaxToken> separators, int[] indicesToRemove)
    {
        var separatorCount = separators.Count;
        var removeLast = indicesToRemove.Contains(separators.Count);

        var newSep = separators.Select((z, i) => new { Item = z, Index = i })
            .Where(z => !indicesToRemove.Contains(z.Index))
            .Select(z => z.Item).ToList();

        if (removeLast && newSep.Count > 0)
        {
            newSep.RemoveAt(newSep.Count - 1);
        }

        return newSep;
    }

    private static SeparatedSyntaxList<TNode> RemoveAtRange<TNode>(SeparatedSyntaxList<TNode> list, IEnumerable<int> indices)
        where TNode : SyntaxNode
    {
        var newContent = list;
        foreach (var z in indices.OrderByDescending(z => z))
        {
            newContent = newContent.RemoveAt(z);
        }

        return newContent;
    }

    private static List<INamedTypeSymbol> GetInterfaces(ITypeSymbol symbol)
    {
        var list = new List<INamedTypeSymbol>(symbol.Interfaces);
        if (symbol.BaseType is { } baseType)
        {
            list.AddRange(GetInterfaces(baseType));
        }

        return list;
    }

    private static BlockSyntax CreateEmptyBody()
        => SyntaxFactory.Block().WithCloseBraceToken(
            SyntaxFactory.Token(SyntaxKind.CloseBraceToken)
            .PrependSpace());

    private static bool ShouldRemoveType(ITypeSymbol symbol)
    {
        if (symbol is IArrayTypeSymbol at)
        {
            return ShouldRemoveType(at.ElementType);
        }

        foreach (var @interface in GetInterfaces(symbol))
        {
            var genericName = GetNameWithoutTypeParams(@interface);
            if (InterfacesToDrop.Contains(genericName))
            {
                return true;
            }
        }

        var @base = GetNameWithoutTypeParams(symbol);
        return Drops.Contains(@base);
    }

    private static ITypeSymbol GetReturnType(ISymbol symbol) => symbol switch
    {
        IFieldSymbol fs => fs.Type,
        IPropertySymbol ps => ps.Type,
        ITypeSymbol ts => ts,
        ILocalSymbol ls => ls.Type,
        IParameterSymbol ps => ps.Type,
        IMethodSymbol ms => ms.ReturnType,
        IDiscardSymbol ds => ds.Type,
        _ => throw new NotSupportedException($"Can't process {symbol}"),
    };

    private static bool ShouldRemoveArgument(ISymbol symbol) => symbol switch
    {
        IPropertySymbol { Name: "CompletedTask", ContainingType.Name: "Task" or "ValueTask" } => true,
        IMethodSymbol ms =>
            IsSpecialMethod(ms) == SpecialMethod.None
                && ((ShouldRemoveType(ms.ReturnType) && ms.MethodKind != MethodKind.LocalFunction)
                    || (ms.ReceiverType is { } receiver && ShouldRemoveType(receiver))),
        _ => ShouldRemoveType(GetReturnType(symbol)),
    };

    private static bool IsTaskExtension(IMethodSymbol methodSymbol)
    {
        var returnType = GetNameWithoutTypeParams(methodSymbol.ReturnType);
        return returnType is ConfiguredTaskAwaitable or ConfiguredValueTaskAwaitable or ConfiguredCancelableAsyncEnumerable;
    }

    private static bool CanDropEmptyStatement(StatementSyntax statement)
        => statement switch
        {
            IfStatementSyntax @if => CanDropIf(@if),
            SwitchStatementSyntax s => CanDropSwitch(s),
            _ => false,
        };

    private static bool EndsWithAsync(ExpressionSyntax expression) => expression switch
    {
        IdentifierNameSyntax id => id.Identifier.ValueText.EndsWithAsync(),
        MemberAccessExpressionSyntax m => EndsWithAsync(m.Name) || EndsWithAsync(m.Expression),
        InvocationExpressionSyntax ie => EndsWithAsync(ie.Expression),
        GenericNameSyntax gn => gn.Identifier.Text.EndsWithAsync(),
        _ => false,
    };

    private static TypeSyntax GetReturnType(TypeSyntax returnType, INamedTypeSymbol symbol)
    {
        var genericReturnType = returnType as GenericNameSyntax;
        var isTask = genericReturnType is null && symbol.ToString() is TaskType or ValueTaskType;

        return isTask
            ? SyntaxFactory.IdentifierName("void")
                .WithTriviaFrom(returnType)
            : returnType;
    }

    private static string Global(string type) => $"global::{type}";

    private static InvocationExpressionSyntax UnwrapExtension(InvocationExpressionSyntax ies, bool isMemory, IMethodSymbol reducedFrom, ExpressionSyntax expression)
    {
        var arguments = ies.ArgumentList.Arguments;
        var separators = arguments.GetSeparators();

        SyntaxToken[] newSeparators = arguments.Count < 1 ? []
            : [Token(SyntaxKind.CommaToken).AppendSpace(), .. separators];

        var @as = Argument(expression);
        var newList = SeparatedList([@as, .. arguments], newSeparators);

        var newName = reducedFrom.Name;
        if (isMemory)
        {
            newName = GetNewName(reducedFrom);
        }
        else
        {
            newName = RemoveAsync(newName);
        }

        var fullyQualifiedName = $"{MakeType(reducedFrom.ContainingType)}.{newName}";

        var es = ies.Expression is MemberAccessExpressionSyntax mae
            ? mae.ChangeIdentifier(fullyQualifiedName)
            : IdentifierName(Identifier(fullyQualifiedName));

        return ies
            .WithExpression(es)
            .WithArgumentList(ArgumentList(newList));
    }

    private static string GetNewName(IMethodSymbol methodSymbol)
    {
        var containingType = methodSymbol.ContainingType;
        var replacement = Regex.Replace(methodSymbol.Name, "Memory", "Span");
        var newSymbol = containingType.GetMembers().FirstOrDefault(z => z.Name == replacement);
        return replacement;
    }

    private bool PreProcess(
        SyntaxList<StatementSyntax> statements,
        Dictionary<int, ExtraNodeInfo> extraNodeInfoList,
        DirectiveStack directiveStack)
    {
        var removeRemaining = false;
        for (var i = 0; i < statements.Count; ++i)
        {
            var statement = statements[i];
            if (ProcessTrivia(statement.GetLeadingTrivia(), directiveStack) is not { } eni)
            {
                return false;
            }

            if (removeRemaining)
            {
                eni = eni with { DropOriginal = true };
            }
            else if (CanDropStatement(statement))
            {
                if (!removeRemaining && statement is ReturnStatementSyntax)
                {
                    removeRemaining = true;
                }

                eni = eni with { DropOriginal = true };
            }

            if (directiveStack.IsSyncOnly == false)
            {
                eni = eni with { DropOriginal = true };
            }

            extraNodeInfoList.Add(i, eni);
        }

        return true;
    }

    private ExtraNodeInfo? ProcessTrivia(SyntaxTriviaList syntaxTriviaList, DirectiveStack directiveStack)
    {
        static SyncOnlyDirectiveType GetDirectiveType(ExpressionSyntax condition) => condition switch
        {
            IdentifierNameSyntax { Identifier.ValueText: SyncOnly } => SyncOnlyDirectiveType.SyncOnly,
            PrefixUnaryExpressionSyntax { Operand: IdentifierNameSyntax { Identifier.ValueText: SyncOnly }, OperatorToken.RawKind: (int)SyntaxKind.ExclamationToken } => SyncOnlyDirectiveType.NotSyncOnly,
            PrefixUnaryExpressionSyntax pue => GetDirectiveType(pue.Operand),
            ParenthesizedExpressionSyntax pe => GetDirectiveType(pe.Expression),
            BinaryExpressionSyntax be
                => GetDirectiveType(be.Left) == SyncOnlyDirectiveType.None
                && GetDirectiveType(be.Right) == SyncOnlyDirectiveType.None
                ? SyncOnlyDirectiveType.None
                : SyncOnlyDirectiveType.Invalid,
            _ => SyncOnlyDirectiveType.None,
        };

        var triviaList = new List<SyntaxTrivia>();

        // Remembers if the last #if was SYNC_ONLY
        var ifDirectives = directiveStack.Stack;

        var statements = new List<StatementSyntax>();
        foreach (var trivia in syntaxTriviaList)
        {
            if (trivia.IsKind(SyntaxKind.IfDirectiveTrivia) && trivia.GetStructure() is IfDirectiveTriviaSyntax ifDirective)
            {
                SyncOnlyDirectiveType syncOnlyDirectiveType = GetDirectiveType(ifDirective.Condition);

                if (syncOnlyDirectiveType == SyncOnlyDirectiveType.Invalid)
                {
                    var d = ReportedDiagnostic.Create(InvalidCondition, trivia.GetLocation(), trivia.ToString());
                    diagnostics.Add(d);
                    return null;
                }

                if (syncOnlyDirectiveType != SyncOnlyDirectiveType.None)
                {
                    if (directiveStack.IsSyncOnly is { } isStackSyncOnly)
                    {
                        if (isStackSyncOnly ^ syncOnlyDirectiveType == SyncOnlyDirectiveType.SyncOnly)
                        {
                            var d = ReportedDiagnostic.Create(InvalidNesting, trivia.GetLocation(), trivia.ToString());
                            diagnostics.Add(d);
                            return null;
                        }
                    }
                    else
                    {
                        directiveStack.IsSyncOnly = syncOnlyDirectiveType == SyncOnlyDirectiveType.SyncOnly;
                    }
                }

                ifDirectives.Push(syncOnlyDirectiveType != SyncOnlyDirectiveType.None);

                if (syncOnlyDirectiveType == SyncOnlyDirectiveType.None)
                {
                    triviaList.Add(trivia);
                }

                continue;
            }

            if (trivia.IsKind(SyntaxKind.ElseDirectiveTrivia))
            {
                if (!ifDirectives.Peek())
                {
                    triviaList.Add(trivia);
                    continue;
                }

                if (directiveStack.IsSyncOnly.HasValue)
                {
                    directiveStack.IsSyncOnly = !directiveStack.IsSyncOnly.Value;
                }

                continue;
            }

            if (trivia.IsKind(SyntaxKind.EndIfDirectiveTrivia))
            {
                if (!ifDirectives.Pop())
                {
                    triviaList.Add(trivia);
                }

                if (ifDirectives.Count == 0)
                {
                    directiveStack.IsSyncOnly = null;
                }

                continue;
            }

            if (trivia.IsKind(SyntaxKind.ElifDirectiveTrivia))
            {
                if (!ifDirectives.Peek())
                {
                    triviaList.Add(trivia);
                }
                else
                {
                    var d = ReportedDiagnostic.Create(InvalidElif, trivia.GetLocation(), trivia.ToString());
                    diagnostics.Add(d);
                    return null;
                }

                continue;
            }

            if (trivia.IsKind(SyntaxKind.DisabledTextTrivia) && directiveStack.IsSyncOnly == true)
            {
                var statementsText = trivia.ToString();
                var compilation = SyntaxFactory.ParseCompilationUnit(statementsText);

                foreach (var m in compilation.Members)
                {
                    if (m is GlobalStatementSyntax gs)
                    {
                        var statement = gs.Statement;
                        if (triviaList.Count > 0)
                        {
                            triviaList.AddRange(statement.GetLeadingTrivia().ToList());
                            statement = statement.WithLeadingTrivia(triviaList);
                            triviaList.Clear();
                        }

                        //Cannot to the following because syntax node is not within syntax tree
                        //var statement = (StatementSyntax)Visit(gs.Statement)!;
                        statements.Add(statement);
                    }
                }

                continue;
            }

            triviaList.Add(trivia);
        }

        return new(false, SyntaxFactory.List(statements), triviaList);
    }

    private ISymbol? GetSymbol(SyntaxNode node) => semanticModel.GetSymbolInfo(node).Symbol;

    private bool CanDropDeclaration(LocalDeclarationStatementSyntax local)
    {
        var symbol = GetSymbol(local.Declaration.Type);

        if (symbol is not null && ShouldRemoveArgument(symbol))
        {
            return true;
        }

        // All variables should have be removed for this declaration to be dropped.
        foreach (var variable in local.Declaration.Variables)
        {
            if (!RemoveDeclarator(variable))
            {
                return false;
            }
        }

        return true;
    }

    private bool RemoveDeclarator(VariableDeclaratorSyntax variable)
        => variable.Initializer is { Value: { } value } && ShouldRemoveArgument(value);

    private TypeSyntax ProcessSyntaxUsingSymbol(TypeSyntax typeSyntax)
    {
        var typeSymbol = semanticModel.GetTypeInfo(typeSyntax).Type;
        if (typeSymbol is null)
        {
            return typeSyntax;
        }

        return ProcessSymbol(typeSymbol).WithTriviaFrom(typeSyntax);
    }

    private TypeSyntax ProcessType(TypeSyntax typeSyntax) => typeSyntax switch
    {
        IdentifierNameSyntax { Identifier.ValueText: "var" } => typeSyntax,
        IdentifierNameSyntax or QualifiedNameSyntax => ProcessSyntaxUsingSymbol(typeSyntax),
        _ => typeSyntax,
    };

    private bool CanDropStatement(StatementSyntax statement) => statement switch
    {
        IfStatementSyntax @if => ShouldRemoveArgument(@if.Condition),
        ExpressionStatementSyntax e => ShouldRemoveArgument(e.Expression),
        LocalDeclarationStatementSyntax l => CanDropDeclaration(l),
        ReturnStatementSyntax { Expression: { } re } => ShouldRemoveArgument(re),
        _ => false,
    };

    private bool HasSymbolAndShouldBeRemoved(ExpressionSyntax expr)
        => GetSymbol(expr) is ISymbol symbol && ShouldRemoveArgument(symbol);

    private bool DropInvocation(InvocationExpressionSyntax invocation)
    {
        var expression = invocation.Expression;
        if (EndsWithAsync(expression))
        {
            return false;
        }

        if (GetSymbol(expression) is not ISymbol symbol)
        {
            return false;
        }

        if (symbol is IMethodSymbol methodSymbol && IsTaskExtension(methodSymbol)
            && expression is MemberAccessExpressionSyntax { Expression: InvocationExpressionSyntax childInvocation })
        {
            return DropInvocation(childInvocation);
        }

        return HasSymbolAndShouldBeRemoved(invocation);
    }

    private bool ShouldRemoveArrowExpression(ArrowExpressionClauseSyntax? arrowNullable)
        => arrowNullable is { } arrow && ShouldRemoveArgument(arrow.Expression);

    private bool ShouldRemoveArgument(ExpressionSyntax expr) => expr switch
    {
        ElementAccessExpressionSyntax ee => ShouldRemoveArgument(ee.Expression),
        BinaryExpressionSyntax be => ShouldRemoveArgument(be.Left) || ShouldRemoveArgument(be.Right),
        CastExpressionSyntax ce => HasSymbolAndShouldBeRemoved(expr) || ShouldRemoveArgument(ce.Expression),
        ParenthesizedExpressionSyntax pe => ShouldRemoveArgument(pe.Expression),
        IdentifierNameSyntax id => !id.Identifier.ValueText.EndsWithAsync() && HasSymbolAndShouldBeRemoved(id),
        InvocationExpressionSyntax ie => DropInvocation(ie),
        ConditionalExpressionSyntax ce => ShouldRemoveArgument(ce.WhenTrue) || ShouldRemoveArgument(ce.WhenFalse),
        MemberAccessExpressionSyntax mae => ShouldRemoveArgument(mae.Name),
        PostfixUnaryExpressionSyntax pue => ShouldRemoveArgument(pue.Operand),
        PrefixUnaryExpressionSyntax pue => ShouldRemoveArgument(pue.Operand),
        ObjectCreationExpressionSyntax oe => ShouldRemoveArgument(oe.Type),
        ConditionalAccessExpressionSyntax cae => ShouldRemoveArgument(cae.Expression),
        AwaitExpressionSyntax ae => ShouldRemoveArgument(ae.Expression),
        AssignmentExpressionSyntax ae => ShouldRemoveArgument(ae.Right),
        GenericNameSyntax gn => HasSymbolAndShouldBeRemoved(gn),
        LiteralExpressionSyntax le => ShouldRemoveLiteral(le),
        _ => false,
    };

    private bool ShouldRemoveLiteral(LiteralExpressionSyntax literalExpression)
        => literalExpression.Token.IsKind(SyntaxKind.DefaultKeyword)
           && semanticModel.GetTypeInfo(literalExpression).Type is INamedTypeSymbol { Name: "ValueTask", IsGenericType: false };

    /// <summary>
    /// Keeps track of nested directives.
    /// </summary>
    private sealed class DirectiveStack()
    {
        /// <summary>
        /// Gets stack values of directive symbols. If SYNC_ONLY or !SYNC_ONLY were the last directive
        /// true is stored, otherwise false.
        /// </summary>
        public Stack<bool> Stack { get; } = new();

        /// <summary>
        /// Gets or sets value indicating whether stack is SYNC_ONLY (true) or !SYNC_ONLY (false).
        /// </summary>
        public bool? IsSyncOnly { get; set; }
    }

    private sealed record ExtraNodeInfo(bool DropOriginal, SyntaxList<StatementSyntax> AdditionalStatements, IList<SyntaxTrivia> LeadingTrivia)
    {
        public ExtraNodeInfo(bool dropOriginal)
            : this(dropOriginal, SyntaxFactory.List(Array.Empty<StatementSyntax>()), Array.Empty<SyntaxTrivia>())
        {
        }

        public static implicit operator ExtraNodeInfo(bool b) => new(b);
    }

    private sealed class StatementProcessor
    {
        private readonly DirectiveStack directiveStack = new();
        private readonly Dictionary<int, ExtraNodeInfo> extraNodeInfoList = [];

        public StatementProcessor(AsyncToSyncRewriter rewriter, SyntaxList<StatementSyntax> statements)
        {
            HasErrors = !rewriter.PreProcess(statements, extraNodeInfoList, directiveStack);
        }

        public bool HasErrors { get; }

        public DirectiveStack DirectiveStack => directiveStack;

        public SyntaxList<StatementSyntax> PostProcess(SyntaxList<StatementSyntax> statements)
        {
            for (var i = 0; i < statements.Count; ++i)
            {
                var statement = statements[i];
                if (CanDropEmptyStatement(statement))
                {
                    if (extraNodeInfoList.TryGetValue(i, out var zz))
                    {
                        extraNodeInfoList[i] = zz with { DropOriginal = true };
                    }
                    else
                    {
                        extraNodeInfoList.Add(i, true);
                    }
                }
            }

            return ProcessStatements(statements, extraNodeInfoList);
        }
    }

    private sealed record ExtensionExprSymbol(InvocationExpressionSyntax InvocationExpression, IMethodSymbol ReducedFrom, ITypeSymbol ReturnType);
}
