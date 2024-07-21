using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Zomp.SyncMethodGenerator;

/// <summary>
/// Rewrites a method synchronously.
/// </summary>
/// <remarks>
/// Creates a new instance of <see cref="AsyncToSyncRewriter"/>.
/// </remarks>
/// <param name="semanticModel">The semantic model.</param>
/// <param name="disableNullable">Instructs the source generator that nullable context should be disabled.</param>
internal sealed class AsyncToSyncRewriter(SemanticModel semanticModel, bool disableNullable) : CSharpSyntaxRewriter
{
    public const string SyncOnly = "SYNC_ONLY";

    // Namespace parts
    private const string Collections = "Collections";
    private const string CompilerServices = "CompilerServices";
    private const string Func = "Func";
    private const string Generic = "Generic";
    private const string System = "System";
    private const string Runtime = "Runtime";
    private const string Threading = "Threading";
    private const string Tasks = "Tasks";

    // Type names
    private const string CancellationToken = "CancellationToken";
    private const string Enumerator = "Enumerator";
    private const string Memory = "Memory";
    private const string ReadOnlyMemory = "ReadOnlyMemory";
    private const string IAsyncEnumerable = "IAsyncEnumerable";
    private const string IAsyncEnumerator = "IAsyncEnumerator";
    private const string IAsyncResult = "IAsyncResult";
    private const string IProgress = "IProgress";
    private const string Object = "object";
    private const string Task = "Task";
    private const string ValueTask = "ValueTask";

    // Members
    private const string AsTask = "AsTask";
    private const string CompletedTask = "CompletedTask";
    private const string Delay = "Delay";
    private const string FromResult = "FromResult";
    private const string IsCancellationRequested = "IsCancellationRequested";
    private const string MoveNextAsync = "MoveNextAsync";
    private const string Span = "Span";

    private const string SystemFunc = $"{System}.{Func}";
    private const string IEnumerable = $"{System}.{Collections}.{Generic}.IEnumerable";
    private const string IEnumerator = $"{System}.{Collections}.{Generic}.IEnumerator";

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

    private readonly SemanticModel semanticModel = semanticModel;
    private readonly bool disableNullable = disableNullable;
    private readonly HashSet<IParameterSymbol> removedParameters = [];
    private readonly Dictionary<string, string> renamedLocalFunctions = [];
    private readonly ImmutableArray<ReportedDiagnostic>.Builder diagnostics = ImmutableArray.CreateBuilder<ReportedDiagnostic>();
    private readonly Stack<ExpressionSyntax> replaceInInvocation = new();
    private readonly DirectiveStack directiveStack = new();

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
        Drop,
    }

    /// <summary>
    /// Gets the diagnostics messages.
    /// </summary>
    public ImmutableArray<ReportedDiagnostic> Diagnostics => diagnostics.ToImmutable();

    /// <inheritdoc/>
    public override SyntaxNode? VisitConditionalAccessExpression(ConditionalAccessExpressionSyntax node)
    {
        var chain = new List<ExtensionExprSymbol>();
        var curNode = node;

        ExtensionExprSymbol? GetExtensionExprSymbol(InvocationExpressionSyntax invocation)
            => GetSymbol(invocation) is not IMethodSymbol
            { IsExtensionMethod: true, ReturnType: { } returnType }
            ? null : new(invocation, returnType);

        while (curNode is { WhenNotNull: { } whenNotNull })
        {
            var ext = whenNotNull switch
            {
                InvocationExpressionSyntax ies => GetExtensionExprSymbol(ies),
                ConditionalAccessExpressionSyntax { Expression: InvocationExpressionSyntax ies } => GetExtensionExprSymbol(ies),
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

        TypeSyntax GetNullableObject()
            => MaybeNullableType(IdentifierName(Object));

        BinaryExpressionSyntax CheckNull(ExpressionSyntax expr) => BinaryExpression(
            SyntaxKind.EqualsExpression,
            CastExpression(GetNullableObject(), expr).AppendSpace(),
            LiteralExpression(SyntaxKind.NullLiteralExpression).PrependSpace());

        var argumentType = GetSymbol(node.Expression) ?? throw new InvalidOperationException("Can't process");
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

            var separated = SeparatedList([MaybeNullableType(argumentTypeExpr), MaybeNullableType(ProcessSymbol(chain[^1].ReturnType))]);

            var type = TypeArgumentList(separated);
            funcExpr = GenericName(
                Identifier(Global(SystemFunc)),
                type);

            toCheckForNullExpr = IdentifierName(parameter);
        }

        ExpressionSyntax lastExpression = null!;

        for (var i = 0; i < chain.Count; i++)
        {
            var (callSymbol, returnType) = chain[i];

            ExpressionSyntax firstArgument = funcArgumentType.IsValueType
                ? MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    toCheckForNullExpr,
                    IdentifierName(nameof(Nullable<int>.Value)))
                : toCheckForNullExpr;
            replaceInInvocation.Push(firstArgument);
            var unwrappedExpr = (InvocationExpressionSyntax)Visit(callSymbol);

            if (i == chain.Count - 1)
            {
                ExpressionSyntax condition = funcArgumentType.IsValueType
                    ? PrefixUnaryExpression(
                        SyntaxKind.LogicalNotExpression,
                        MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            toCheckForNullExpr,
                            IdentifierName(nameof(Nullable<int>.HasValue))))
                    : CheckNull(toCheckForNullExpr);
                var castTo = MaybeNullableType(ProcessSymbol(returnType), returnType.IsValueType);

                var conditional = ConditionalExpression(
                    condition.AppendSpace(),
                    CastExpression(castTo, LiteralExpression(SyntaxKind.NullLiteralExpression)).PrependSpace().AppendSpace(),
                    unwrappedExpr.PrependSpace());

                lastExpression = conditional;

                statements.Add(ReturnStatement(conditional.PrependSpace()));
                continue;
            }

            var returnNullStatement = ReturnStatement(LiteralExpression(SyntaxKind.NullLiteralExpression).PrependSpace());

            var ifBlock = ((ICollection<StatementSyntax>)[returnNullStatement]).CreateBlock(3);
            var ifStatement = IfStatement(Token(SyntaxKind.IfKeyword), Token(SyntaxKind.OpenParenToken).PrependSpace(), CheckNull(toCheckForNullExpr), Token(SyntaxKind.CloseParenToken), ifBlock, null);
            statements.Add(ifStatement);

            var toCheckForNull = Identifier($"check{i}");
            var localType = ProcessSymbol(returnType); // reduced will return generic

            var declarator = VariableDeclarator(toCheckForNull.AppendSpace(), null, EqualsValueClause(unwrappedExpr.PrependSpace()));
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

        return TypeAlreadyQualified(node.ElementType) ? @base : (SyntaxNode)@base.WithElementType(ProcessType(@base.ElementType)).WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitGenericName(GenericNameSyntax node)
    {
        var @base = (GenericNameSyntax)base.VisitGenericName(node)!;

        if (GetSymbol(node) is not INamedTypeSymbol symbol)
        {
            return @base;
        }

        static string GetIdentifier(INamedTypeSymbol symbol)
            => GetReplacement(symbol) is { } replacement
                ? Global(replacement)
                : symbol switch
                {
                    { ContainingSymbol: INamedTypeSymbol { IsGenericType: true } parentSymbol }
                    => parentSymbol.ToDisplayString(GlobalDisplayFormatWithTypeParameters) + "." + symbol.Name,
                    _ => symbol.ToDisplayString(GlobalDisplayFormat),
                };

        var replacement = symbol switch
        {
            {
                Name: Task or ValueTask, IsGenericType: true,
                ContainingNamespace: { Name: Tasks, ContainingNamespace: { Name: Threading, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
            }

            => @base.TypeArgumentList.Arguments[0],
            _ => @base.WithIdentifier(Identifier(GetIdentifier(symbol))),
        };

        return replacement.WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitIdentifierName(IdentifierNameSyntax node)
    {
        var @base = (IdentifierNameSyntax)base.VisitIdentifierName(node)!;
        if (renamedLocalFunctions.TryGetValue(@base.Identifier.ValueText, out var newName))
        {
            return @base.WithIdentifier(Identifier(newName));
        }
        else if (node.Parent is not MemberAccessExpressionSyntax
            && GetSymbol(node) is IFieldSymbol { IsStatic: true } fieldSymbol)
        {
            var typeString = fieldSymbol.ContainingType.ToDisplayString(GlobalDisplayFormatWithTypeParameters);
            return @base.WithIdentifier(Identifier($"{typeString}.{fieldSymbol.Name}"));
        }
        else if (node.Parent is TypeArgumentListSyntax)
        {
            return ProcessType(node);
        }

        return @base;
    }

    public override SyntaxNode? VisitQualifiedName(QualifiedNameSyntax node)
    {
        var @base = (QualifiedNameSyntax)base.VisitQualifiedName(node)!;

        return @base.Right is GenericNameSyntax ? @base.Right : (SyntaxNode)ProcessType(node);
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
            newNode = @base.WithIdentifier(Identifier(newName));
        }

        var nonEmptyAttributes = List(@base.AttributeLists.Where(z => z.Attributes.Any()));

        return newNode
            .WithReturnType(GetReturnType(@base.ReturnType, symbol))
            .WithModifiers(StripAsyncModifier(@base.Modifiers))
            .WithAttributeLists(nonEmptyAttributes)
            .WithTriviaFrom(@base);
    }

    public override SyntaxNode? VisitParameterList(ParameterListSyntax node)
    {
        var newNode = (ParameterListSyntax)base.VisitParameterList(node)!;
        var modifications = new Dictionary<int, Operation>();

        var ds = new DirectiveStack();
        bool IsValidParameter(ParameterSyntax ps, int i)
        {
            var leading = ps.GetLeadingTrivia();
            var extra = ProcessTrivia(leading, ds);
            if (extra is not null && extra.AdditionalStatements.Count > 0)
            {
                modifications.Add(i, new List<StatementSyntax>(extra.AdditionalStatements));
                modifications.Add(i + 1, true);
            }

            if (semanticModel.GetDeclaredSymbol(ps) is not { } symbol)
            {
                return true;
            }

            var nts = symbol.Type;

            if (ShouldRemoveType(nts))
            {
                _ = removedParameters.Add(symbol);
                return false;
            }

            return true;
        }

        var invalid = node.Parameters.GetIndices((v, i) => !IsValidParameter(v, i));
        var newParams = RemoveAtRange(newNode.Parameters, invalid);

        var entries
            = modifications.OrderByDescending(z => z.Key);

        var removeTrailingEndIf = false;

        List<SyntaxTrivia> RemoveFirstEndIf(SyntaxTriviaList list)
        {
            var newLeadingTrivia = new List<SyntaxTrivia>();

            var removed = false;
            foreach (var st in list)
            {
                if (!removed && st.IsKind(SyntaxKind.EndIfDirectiveTrivia))
                {
                    removed = true;
                    newLeadingTrivia.Add(ElasticCarriageReturnLineFeed);
                    continue;
                }

                newLeadingTrivia.Add(st);
            }

            return newLeadingTrivia;
        }

        foreach (var extraParameterGroup in entries)
        {
            var index = extraParameterGroup.Key;

            if (extraParameterGroup.Value.IsNewStatements)
            {
                newParams = newParams.RemoveAt(index);
                foreach (var extraParameter in extraParameterGroup.Value.AsNewStatements)
                {
                    if (extraParameter is not LocalDeclarationStatementSyntax declaration)
                    {
                        continue;
                    }

                    var id = Identifier(declaration.Declaration.Variables.Single(v => !string.IsNullOrWhiteSpace(v.Identifier.ValueText)).Identifier.ValueText);
                    var p = Parameter(default, default, declaration.Declaration.Type, id, default);
                    newParams = newParams.Insert(index, p);
                }
            }
            else
            {
                if (index >= newParams.Count)
                {
                    removeTrailingEndIf = true;
                    continue;
                }

                var pRemoveEndIf = newParams[index];
                newParams = newParams.RemoveAt(index);
                var leadingTrivia = pRemoveEndIf.GetLeadingTrivia();

                var newLeadingTrivia = RemoveFirstEndIf(leadingTrivia);

                pRemoveEndIf = pRemoveEndIf.WithLeadingTrivia(newLeadingTrivia);
                newParams = newParams.Insert(index, pRemoveEndIf);
            }
        }

        newNode = newNode.WithParameters(newParams);

        if (removeTrailingEndIf)
        {
            newNode = newNode.WithCloseParenToken(newNode.CloseParenToken.WithLeadingTrivia(RemoveFirstEndIf(newNode.CloseParenToken.LeadingTrivia)));
        }

        return newNode;
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitParameter(ParameterSyntax node)
    {
        var @base = (ParameterSyntax)base.VisitParameter(node)!;

        return node.Type is { } originalType && @base.Type is not null

            // If type is generic
            && GetSymbol(originalType) is INamedTypeSymbol { IsGenericType: true } namedTypeSymbol

            // And it is System.Func
            && IsSystemFunc(namedTypeSymbol)

            // And can be converter to Action
            && ConvertFuncToAction(@base.Type, namedTypeSymbol) is { } newTypeSyntax
            ? @base.WithType(newTypeSyntax.WithTriviaFrom(originalType))
            : (node.Type is null || TypeAlreadyQualified(node.Type) ? @base
            : @base.WithType(ProcessType(node.Type)).WithTriviaFrom(@base));
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
    {
        var @base = (MemberAccessExpressionSyntax)base.VisitMemberAccessExpression(node)!;

        if (GetSymbol(node.Expression) is not { } exprSymbol)
        {
            return @base;
        }

        if (exprSymbol is ITypeSymbol && node.Expression is TypeSyntax type)
        {
            // Rewrite static invocation (eg. File.ReadAllTextAsync)
            var newType = ProcessType(type);
            if (newType != type)
            {
                @base = @base.WithExpression(newType);
            }
        }

        if (@base.Name.Identifier.ValueText == Span
            && GetReturnType(exprSymbol) is INamedTypeSymbol
            {
                Name: Memory or ReadOnlyMemory, IsGenericType: true,
                ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true }
            })
        {
            return @base.Expression;
        }
        else if (@base.Name.Identifier.ValueText.EndsWithAsync())
        {
            return @base.WithName(@base.ChangeIdentifier(RemoveAsync(@base.Name.Identifier.ValueText)));
        }

        return GetSymbol(node) is IPropertySymbol property
            && property.Type is INamedTypeSymbol
            {
                Name: Memory or ReadOnlyMemory, IsGenericType: true,
                ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true }
            }

            ? AppendSpan(@base)
            : @base;
    }

    public override SyntaxNode? VisitUsingStatement(UsingStatementSyntax node)
    {
        var @base = (UsingStatementSyntax)base.VisitUsingStatement(node)!;
        return @base.WithAwaitKeyword(default);
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

            return @base.WithExpression(IdentifierName(newName));
        }
        else if (@base.Expression is GenericNameSyntax gn && gn.Identifier.Text.EndsWithAsync())
        {
            newName = RemoveAsync(gn.Identifier.Text);
            return @base.WithExpression(gn.WithIdentifier(Identifier(newName)));
        }

        var isMemory = methodSymbol.ReturnType is INamedTypeSymbol
        {
            Name: Memory or ReadOnlyMemory, IsGenericType: true,
            ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true }
        };

        if (isMemory)
        {
            newName = GetNewName(methodSymbol);
        }

        var reducedFromExtensionMethod = methodSymbol.IsExtensionMethod ? methodSymbol.ReducedFrom : null;

        // Handle non null conditional access expression eg. arr?.First()
        if (@base.Expression is MemberBindingExpressionSyntax
            && reducedFromExtensionMethod is not null
            && replaceInInvocation.Count > 0)
        {
            return UnwrapExtension(@base, isMemory, reducedFromExtensionMethod, replaceInInvocation.Pop());
        }

        if (@base.Expression is not MemberAccessExpressionSyntax { } memberAccess)
        {
            return isMemory ? AppendSpan(@base) : @base;
        }

        if (methodSymbol.ReturnType is INamedTypeSymbol
            {
                Name: Enumerator, ContainingType:
                {
                    IsGenericType: true, TypeArguments: [{ }],
                    ContainingNamespace: { Name: CompilerServices, ContainingNamespace: { Name: Runtime, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
                }
            })
        {
            return @base.WithExpression(MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, memberAccess.Expression, IdentifierName(nameof(IEnumerable.GetEnumerator)))).WithArgumentList(ArgumentList([]));
        }

        // Handle .ConfigureAwait(), .WithCancellationToken() and return the part in front of it
        if (IsTaskExtension(methodSymbol) && methodSymbol.Name is not MoveNextAsync)
        {
            return memberAccess.Expression;
        }

        // Handle all other extension methods eg. arr.First()
        if (reducedFromExtensionMethod is not null)
        {
            return UnwrapExtension(@base, isMemory, reducedFromExtensionMethod, memberAccess.Expression);
        }

        if (memberAccess.Name is not SimpleNameSyntax { Identifier.ValueText: { } name })
        {
            return @base;
        }

        if (name.EndsWithAsync()
            || methodSymbol.ReturnType is INamedTypeSymbol
            {
                Name: IAsyncEnumerator, IsGenericType: true,
                ContainingNamespace: { Name: Generic, ContainingNamespace: { Name: Collections, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
            })
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
            else if (specialMethod == SpecialMethod.Drop)
            {
                return memberAccess.Expression;
            }
        }

        return newName is null ? @base : @base.WithExpression(memberAccess.WithName(IdentifierName(newName)));
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

    /// <inheritdoc/>
    public override SyntaxNode? VisitAnonymousMethodExpression(AnonymousMethodExpressionSyntax node)
    {
        var @base = (AnonymousMethodExpressionSyntax)base.VisitAnonymousMethodExpression(node)!;
        return @base.WithModifiers(StripAsyncModifier(@base.Modifiers));
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitInterpolation(InterpolationSyntax node)
    {
        var @base = (InterpolationSyntax)base.VisitInterpolation(node)!;
        if (@base.Expression is not ParenthesizedExpressionSyntax)
        {
            var newExpression = ParenthesizedExpression(@base.Expression);
            @base = @base.WithExpression(newExpression);
        }

        return @base;
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitImplicitObjectCreationExpression(ImplicitObjectCreationExpressionSyntax node)
    {
        var @base = base.VisitImplicitObjectCreationExpression(node);
        var symbol = GetSymbol(node);

        return TryReplaceObjectCreation(node, symbol, out var replacement) ? replacement : @base;
    }

    public override SyntaxNode? VisitReturnStatement(ReturnStatementSyntax node)
    {
        // Replace expressions that return the task directly.
        if (node is { Expression: { } returnExpression } &&
            semanticModel.GetTypeInfo(returnExpression).Type is INamedTypeSymbol named && IsTaskOrValueTask(named))
        {
            var result = ExpressionToStatement(returnExpression);

            if (result is not null && node.Parent is not BlockSyntax)
            {
                // The parent is not a block, for example: if (true) return ReturnAsync();
                // We need to create a block with the expression and the return statement.
                return Block(List(new StatementSyntax[]
                    {
                        result.WithLeadingTrivia(Space).WithTrailingTrivia(Space),
                        ReturnStatement().WithTrailingTrivia(Space),
                    }))
                    .WithLeadingTrivia(node.GetLeadingTrivia())
                    .WithTrailingTrivia(node.GetTrailingTrivia());
            }

            // Don't return if the return statement is the last statement in the method.
            if (node.Parent?.Parent is MethodDeclarationSyntax { Body.Statements: [.., var lastStatement] } &&
                lastStatement == node)
            {
                return result is null
                    ? null
                    : (SyntaxNode)result
                    .WithLeadingTrivia(node.GetLeadingTrivia())
                    .WithTrailingTrivia(node.GetTrailingTrivia());
            }

            if (result is null)
            {
                return ReturnStatement()
                    .WithTrailingTrivia(node.GetTrailingTrivia())
                    .WithLeadingTrivia(node.GetLeadingTrivia());
            }

            // Create a block without the braces (eg. Return(); return;)
            return Block(List(new StatementSyntax[]
                {
                    result.WithTrailingTrivia(Space),
                    ReturnStatement().WithTrailingTrivia(node.GetTrailingTrivia()),
                }))
                .WithOpenBraceToken(MissingToken(SyntaxKind.OpenBraceToken))
                .WithCloseBraceToken(MissingToken(SyntaxKind.CloseBraceToken));
        }

        return base.VisitReturnStatement(node)!;
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitObjectCreationExpression(ObjectCreationExpressionSyntax node)
    {
        var @base = (ObjectCreationExpressionSyntax)base.VisitObjectCreationExpression(node)!;
        var symbol = GetSymbol(node);

        if (TryReplaceObjectCreation(node, symbol, out var replacement))
        {
            return replacement;
        }

        if (semanticModel.GetTypeInfo(node).Type is not { } t
            || t is INamedTypeSymbol { IsGenericType: true })
        {
            return @base;
        }

        var newType = ProcessSymbol(t);
        return newType == node.Type ? @base : @base.WithType(newType);
    }

    public override SyntaxNode? VisitIfStatement(IfStatementSyntax node)
    {
        var substituteIfWithEmpty = node.Statement is ExpressionStatementSyntax es && ShouldRemoveArgument(es.Expression);
        var substituteElseWithEmpty = node.Else is ElseClauseSyntax { Statement: ExpressionStatementSyntax { Expression: { } e } } && ShouldRemoveArgument(e);

        var retVal = (IfStatementSyntax)base.VisitIfStatement(node)!;

        if (substituteIfWithEmpty)
        {
            retVal = retVal.WithStatement(Block());
        }

        if (substituteElseWithEmpty)
        {
            retVal = retVal.WithElse(ElseClause(Block()));
        }

        if (ChecksIfNegatedIsCancellationRequested(node.Condition))
        {
            retVal = retVal.WithCondition(LiteralExpression(SyntaxKind.TrueLiteralExpression));
        }

        return retVal;
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitSwitchSection(SwitchSectionSyntax node)
    {
        var statementProcessor = new StatementProcessor(this, node.Statements, directiveStack);
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
        var statementProcessor = new StatementProcessor(this, node.Statements, directiveStack);
        if (statementProcessor.HasErrors)
        {
            return node;
        }

        var @base = (BlockSyntax)base.VisitBlock(node)!;

        var newStatements = statementProcessor.PostProcess(@base.Statements);
        var retVal = @base.WithStatements(newStatements);

        var lastToken = retVal.CloseBraceToken;
        if (ProcessTrivia(node.CloseBraceToken.LeadingTrivia, directiveStack) is var (_, newStatements2, newTrivia))
        {
            var oldStatements = retVal.Statements.ToList();
            oldStatements.AddRange([.. newStatements2]);
            retVal = retVal.WithStatements(List(oldStatements)).WithCloseBraceToken(lastToken.WithLeadingTrivia(newTrivia));
        }

        return retVal;
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitTypeArgumentList(TypeArgumentListSyntax node)
    {
        // Ensure that Func<Task<T>> is not removed
        var isFunc = node.Parent is { } parent
            && semanticModel.GetTypeInfo(parent).Type is INamedTypeSymbol { IsGenericType: true } n
            && IsSystemFunc(n);

        // Do not remove Task<T>, but remove Task inside a Func<>
        bool RemoveType(TypeSyntax z, int index) =>
            !(isFunc && index == node.Arguments.Count - 1 && z is GenericNameSyntax)
            && semanticModel.GetTypeInfo(z).Type is { } type
            && ShouldRemoveType(type);

        var indicesToRemove = node.Arguments.GetIndices(RemoveType);

        var @base = (TypeArgumentListSyntax)base.VisitTypeArgumentList(node)!;
        var newSep = RemoveSeparators(@base.Arguments.GetSeparators().ToList(), indicesToRemove);

        var newArguments = SeparatedList(
            RemoveAtRange(@base.Arguments, indicesToRemove),
            newSep);
        return @base.WithArguments(newArguments);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitTypeConstraint(TypeConstraintSyntax node)
    {
        var @base = (TypeConstraintSyntax)base.VisitTypeConstraint(node)!;
        var newType = ProcessType(@base.Type);
        return newType == @base.Type ? @base : (SyntaxNode)@base.WithType(newType).WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitCatchDeclaration(CatchDeclarationSyntax node)
    {
        var @base = (CatchDeclarationSyntax)base.VisitCatchDeclaration(node)!;
        return @base.WithType(ProcessType(node.Type)).WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitGlobalStatement(GlobalStatementSyntax node)
    {
        var @base = (GlobalStatementSyntax)base.VisitGlobalStatement(node)!;
        return @base;
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

        var isTask = genericReturnType is null && IsTaskOrValueTask(symbol);

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
            var newTrivia = Trivia(comment);
            newTriviaList = trivia.Replace(comments, newTrivia);
        }

        static bool Preprocessors(SyntaxTrivia st)
            => st.IsKind(SyntaxKind.IfDirectiveTrivia)
            || st.IsKind(SyntaxKind.ElifDirectiveTrivia)
            || st.IsKind(SyntaxKind.ElseDirectiveTrivia)
            || st.IsKind(SyntaxKind.EndIfDirectiveTrivia)
            || st.IsKind(SyntaxKind.RegionDirectiveTrivia)
            || st.IsKind(SyntaxKind.EndRegionDirectiveTrivia)
            || st.IsKind(SyntaxKind.DisabledTextTrivia);

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

        var attrLists = new List<AttributeListSyntax>();
        foreach (var l in @base.AttributeLists)
        {
            var leading = l.OpenBracketToken.LeadingTrivia;
            if (ProcessSyncOnlyAttributes(leading, new())
                is { Attributes.Count: > 0 } additionalAttributes2)
            {
                attrLists.AddRange(additionalAttributes2.Attributes);
            }

            if (l.Attributes.Count > 0)
            {
                attrLists.Add(l);
            }
        }

        var nonEmptyAttributes = List(attrLists);
        if (node.Modifiers is [var first, ..]
            && ProcessSyncOnlyAttributes(first.LeadingTrivia, new())
            is { Attributes.Count: > 0 } additionalAttributes)
        {
            nonEmptyAttributes = List(nonEmptyAttributes.Union(additionalAttributes.Attributes));
            var m = TokenList([first.WithLeadingTrivia(additionalAttributes.LeadingTrivia), .. @base.Modifiers.Skip(1)]);
            @base = @base.WithModifiers(m);
        }

        var retVal = @base
            .WithIdentifier(Identifier(newName))
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
        return GetSymbol(node.Expression) is ITypeSymbol { } typeSymbol
            && !TypeAlreadyQualified(typeSymbol)
            ? @base.WithExpression(ProcessSymbol(typeSymbol)).WithTriviaFrom(@base)
            : @base;
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
        return TypeAlreadyQualified(node.Type)
            ? @base.WithAwaitKeyword(default)
            : @base.WithAwaitKeyword(default).WithType(ProcessType(node.Type)).WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitForEachVariableStatement(ForEachVariableStatementSyntax node)
    {
        var @base = (ForEachVariableStatementSyntax)base.VisitForEachVariableStatement(node)!;

        return @base.WithAwaitKeyword(default).WithTriviaFrom(@base);
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
    public override SyntaxNode? VisitWhileStatement(WhileStatementSyntax node)
    {
        var @base = (WhileStatementSyntax)base.VisitWhileStatement(node)!;

        return ChecksIfNegatedIsCancellationRequested(node.Condition)
            ? @base.WithCondition(LiteralExpression(SyntaxKind.TrueLiteralExpression))
            : @base;
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
        if (variableType is INamedTypeSymbol { IsGenericType: true } namedTypeSymbol
            && IsSystemFunc(namedTypeSymbol))
        {
            var newVariableNames = new List<VariableDeclaratorSyntax>();
            foreach (var variable in @base.Declaration.Variables)
            {
                newVariableNames.Add(VariableDeclarator(RemoveAsync(variable.Identifier.Text))
                    .WithTrailingTrivia(variable.Identifier.TrailingTrivia).WithInitializer(variable.Initializer));
            }

            var typeArgs = namedTypeSymbol.TypeArguments;

            if (ConvertFuncToAction(@base.Declaration.Type, namedTypeSymbol) is { } newTypeSyntax2)
            {
                newTypeSyntax = newTypeSyntax2;
            }

            if (typeArgs[^1] is INamedTypeSymbol named && IsTaskOrValueTask(named)
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

                    var separatedList = SeparatedList(list, originalSeparators);
                    newTypeSyntax = GenericName(Identifier(newType), TypeArgumentList(separatedList));
                }
                else
                {
                    newTypeSyntax = IdentifierName(newType);
                }

                newTypeSyntax = newTypeSyntax.WithTriviaFrom(node.Declaration.Type);
            }

            separatedVariableNames = SeparatedList(newVariableNames, node.Declaration.Variables.GetSeparators());
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

    public override SyntaxNode? VisitVariableDeclarator(VariableDeclaratorSyntax node)
    {
        var @base = (VariableDeclaratorSyntax)base.VisitVariableDeclarator(node)!;
        return @base;
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitVariableDeclaration(VariableDeclarationSyntax node)
    {
        ////((Microsoft.CodeAnalysis.CSharp.Syntax.ObjectCreationExpressionSyntax)node.Variables[0].Initializer.Value).Type

        ////TypeSyntax? GetInitializerType(VariableDeclarationSyntax node)
        ////    => (node.Variables[0].Initializer?.Value as ObjectCreationExpressionSyntax)?.Type;

        ////var isEqual = node.Type == GetInitializerType(node);

        var @base = (VariableDeclarationSyntax)base.VisitVariableDeclaration(node)!;
        var type = node.Type;
        var newType = @base.Type;

        if (newType == type ||
            (newType is IdentifierNameSyntax { Identifier.ValueText: { } newTypeString }
            && type is IdentifierNameSyntax { Identifier.ValueText: { } typeString }
            && newTypeString == typeString))
        {
            // not replaced
            newType = ProcessType(type);

            if (newType == type)
            {
                return @base;
            }
        }

        //return @base.WithType(ProcessType(type)).WithTriviaFrom(@base);
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

        /*
        if (ProcessSyncOnlyAttributes(@base.OpenBracketToken.LeadingTrivia, new())
            is { Attributes.Count: > 0 } additionalAttributes)
        {
            newList.AddRange();
        }
        */

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

    public override SyntaxNode? VisitConstantPattern(ConstantPatternSyntax node)
    {
        var @base = (ConstantPatternSyntax)base.VisitConstantPattern(node)!;
        return semanticModel.GetTypeInfo(node.Expression).Type is { } type
            ? @base.WithExpression(ProcessSymbol(type).WithTriviaFrom(@base))
            : @base;
    }

    public override SyntaxNode? VisitDeclarationExpression(DeclarationExpressionSyntax node)
    {
        var @base = (DeclarationExpressionSyntax)base.VisitDeclarationExpression(node)!;
        return TypeAlreadyQualified(node.Type) ? @base : (SyntaxNode)@base.WithType(ProcessType(node.Type)).WithTriviaFrom(@base);
    }

    public override SyntaxNode? VisitCastExpression(CastExpressionSyntax node)
    {
        var @base = (CastExpressionSyntax)base.VisitCastExpression(node)!;
        return TypeAlreadyQualified(node.Type) ? @base : (SyntaxNode)@base.WithType(ProcessType(node.Type)).WithTriviaFrom(@base);
    }

    public override SyntaxNode? VisitTupleType(TupleTypeSyntax node)
    {
        var @base = (TupleTypeSyntax)base.VisitTupleType(node)!;

        var newTuples = new List<TupleElementSyntax>();
        foreach (var t in node.Elements.Zip(@base.Elements, (original, visited) => (original, visited)))
        {
            var newType = TypeAlreadyQualified(t.original.Type) ? t.visited.Type : ProcessType(t.original.Type);
            newTuples.Add(TupleElement(newType, t.original.Identifier));
        }

        return @base.WithElements(SeparatedList(newTuples, node.Elements.GetSeparators()));
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

        if ((@base.OperatorToken.IsKind(SyntaxKind.IsKeyword) || @base.OperatorToken.IsKind(SyntaxKind.AsKeyword))
            && GetSymbol(node.Right) is INamedTypeSymbol typeSymbol)
        {
            @base = @base.WithRight(ProcessSymbol(typeSymbol)).WithTriviaFrom(@base);
        }

        return @base;
    }

    internal static bool IsTypeOfInterest(INamedTypeSymbol symbol) => symbol switch
    {
        {
            Name: Task or ValueTask,
            ContainingNamespace: { Name: Tasks, ContainingNamespace: { Name: Threading, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
        }

        => true,
        {
            Name: Enumerator, ContainingType:
            {
                IsGenericType: true, TypeArguments: [{ }],
                ContainingNamespace: { Name: CompilerServices, ContainingNamespace: { Name: Runtime, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
            }
        }

        => true,
        {
            Name: Memory or ReadOnlyMemory, IsGenericType: true,
            ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true }
        }

        => true,
        {
            Name: IAsyncEnumerable or IAsyncEnumerator, IsGenericType: true,
            ContainingNamespace: { Name: Generic, ContainingNamespace: { Name: Collections, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
        }

        => true,
        _ => false,
    };

    private static string MakeType(ISymbol symbol)
        => symbol switch
        {
            INamedTypeSymbol { Name: "AsyncEnumerable" } => Global("System.Linq.Enumerable"),
            INamedTypeSymbol => symbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat),
            _ => symbol.Name,
        };

    private static SimpleNameSyntax ProcessSymbol(ITypeSymbol typeSymbol) => typeSymbol switch
    {
        INamedTypeSymbol
        {
            Name: Enumerator, ContainingType:
            {
                IsGenericType: true, TypeArguments: [var type],
                ContainingNamespace: { Name: CompilerServices, ContainingNamespace: { Name: Runtime, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
            }
        }

        => GenericName(IEnumerator).WithTypeArgumentList(TypeArgumentList(SeparatedList<TypeSyntax>([ProcessSymbol(type)], []))),
        INamedTypeSymbol
        {
            Name: nameof(Task) or nameof(ValueTask),
            ContainingNamespace: { Name: Tasks, ContainingNamespace: { Name: Threading, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
        }

        symbol => symbol.IsGenericType ? ProcessSymbol(symbol.TypeArguments[0]) : IdentifierName("void"),
        INamedTypeSymbol nts => IdentifierName(MakeType(nts)),
        IArrayTypeSymbol ats => IdentifierName(MakeType(ats.ElementType) + "[]"),
        _ => IdentifierName(typeSymbol.Name),
    };

    private static SpecialMethod IsSpecialMethod(IMethodSymbol methodSymbol) => methodSymbol switch
    {
        {
            Name: Delay, ContainingType:
            {
                Name: Task or ValueTask, IsGenericType: false,
                ContainingNamespace: { Name: Tasks, ContainingNamespace: { Name: Threading, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
            }
        }

        => SpecialMethod.Delay,
        {
            Name: FromResult, ContainingType:
            {
                Name: Task or ValueTask, IsGenericType: false,
                ContainingNamespace: { Name: Tasks, ContainingNamespace: { Name: Threading, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
            }
        }

        => SpecialMethod.FromResult,
        {
            Name: AsTask, ContainingType:
            {
                Name: ValueTask,
                ContainingNamespace: { Name: Tasks, ContainingNamespace: { Name: Threading, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
            }
        }

        => SpecialMethod.Drop,
        _ => SpecialMethod.None,
    };

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
            else if (extra is { LeadingTrivia: { } lt })
            {
                if (lt.Any(st => st.IsKind(SyntaxKind.IfDirectiveTrivia)))
                {
                    newStatements.Add(EmptyStatement().WithSemicolonToken(MissingToken(SyntaxKind.SemicolonToken)).WithLeadingTrivia(lt));
                }
            }
        }

        return List(newStatements);
    }

    private static SyntaxTokenList StripAsyncModifier(SyntaxTokenList list)
        => TokenList(list.Where(z => !z.IsKind(SyntaxKind.AsyncKeyword)));

    private static string RemoveAsync(string original)
        => Regex.Replace(original, "Async", string.Empty);

    private static string ReplaceWithSpan(string original)
        => Regex.Replace(original, "Memory", Span);

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

    private static string? GetReplacement(INamedTypeSymbol symbol) => symbol switch
    {
        {
            Name: Memory or ReadOnlyMemory, IsGenericType: true,
            ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true }
        }

        => $"{System}.{ReplaceWithSpan(symbol.Name)}",
        {
            Name: IAsyncEnumerable or IAsyncEnumerator, IsGenericType: true,
            ContainingNamespace: { Name: Generic, ContainingNamespace: { Name: Collections, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
        }

        => $"{System}.{Collections}.{Generic}.{RemoveAsync(symbol.Name)}",
        _ => null,
    };

    private static BlockSyntax CreateEmptyBody()
        => Block().WithCloseBraceToken(
            Token(SyntaxKind.CloseBraceToken)
            .PrependSpace());

    private static bool ShouldRemoveType(ITypeSymbol symbol)
    {
        if (symbol is IArrayTypeSymbol at)
        {
            return ShouldRemoveType(at.ElementType);
        }

        if (symbol is not INamedTypeSymbol namedSymbol)
        {
            return false;
        }

        foreach (var @interface in GetInterfaces(namedSymbol))
        {
            if (IsIProgress(@interface) || @interface is
                {
                    Name: IAsyncResult, IsGenericType: false,
                    ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true }
                })
            {
                return true;
            }
        }

        return IsIProgress(namedSymbol) || IsCancellationToken(namedSymbol);
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
        IEventSymbol es => es.Type,
        _ => throw new NotSupportedException($"Can't process {symbol}"),
    };

    private static bool ShouldRemoveArgument(ISymbol symbol, bool isNegated = false) => symbol switch
    {
        IPropertySymbol
        {
            Name: CompletedTask, Type: INamedTypeSymbol
            {
                Name: Task or ValueTask, IsGenericType: false,
                ContainingNamespace: { Name: Tasks, ContainingNamespace: { Name: Threading, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
            }
        }

        => true,
        IPropertySymbol
        {
            Name: IsCancellationRequested, ContainingType:
            {
                Name: CancellationToken, IsGenericType: false,
                ContainingNamespace: { Name: Threading, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } }
            }
        }

        => !isNegated,
        IMethodSymbol ms =>
            IsSpecialMethod(ms) == SpecialMethod.None
                && ((ShouldRemoveType(ms.ReturnType) && ms.MethodKind != MethodKind.LocalFunction)
                    || (ms.ReceiverType is { } receiver && ShouldRemoveType(receiver))),
        _ => ShouldRemoveType(GetReturnType(symbol)),
    };

    private static MemberAccessExpressionSyntax AppendSpan(ExpressionSyntax @base)
        => MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, @base, IdentifierName(Span));

    private static ExpressionSyntax RemoveParentheses(ExpressionSyntax condition)
    {
        var node = condition;
        while (node is ParenthesizedExpressionSyntax p)
        {
            node = p.Expression;
        }

        return node;
    }

    private static bool IsTaskExtension(IMethodSymbol methodSymbol) => methodSymbol.ReturnType is
    {
        Name: "ConfiguredTaskAwaitable" or "ConfiguredValueTaskAwaitable" or "ConfiguredCancelableAsyncEnumerable" or "ConfiguredAsyncDisposable",
        ContainingNamespace: { Name: CompilerServices, ContainingNamespace: { Name: Runtime, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
    };

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

    private static TypeSyntax GetReturnType(TypeSyntax returnType, INamedTypeSymbol symbol) => (symbol switch
    {
        _ when returnType is GenericNameSyntax => returnType,
        _ => ProcessSymbol(symbol),
    }).WithTriviaFrom(returnType);

    private static string Global(string type) => $"global::{type}";

    private static bool TypeAlreadyQualified(TypeSyntax type)
        => type is NullableTypeSyntax or GenericNameSyntax or TupleTypeSyntax or ArrayTypeSyntax or QualifiedNameSyntax;

    private static bool TypeAlreadyQualified(ITypeSymbol type)
        => type is INamedTypeSymbol namedType
            && namedType is { IsGenericType: true };

    private static bool TryReplaceObjectCreation(BaseObjectCreationExpressionSyntax node, ISymbol? symbol, out SyntaxNode? replacement)
    {
        if (symbol is IMethodSymbol
            {
                ReceiverType: INamedTypeSymbol
                {
                    Name: ValueTask, IsGenericType: true,
                    ContainingNamespace: { Name: Tasks, ContainingNamespace: { Name: Threading, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
                }
            }

            && node.ArgumentList is { Arguments: [var singleArg] })
        {
            replacement = singleArg.Expression;
            return true;
        }

        replacement = default;
        return false;
    }

    private static InvocationExpressionSyntax UnwrapExtension(InvocationExpressionSyntax ies, bool isMemory, IMethodSymbol reducedFrom, ExpressionSyntax expression)
    {
        var arguments = ies.ArgumentList.Arguments;
        var separators = arguments.GetSeparators();

        SyntaxToken[] newSeparators = arguments.Count < 1 ? []
            : [Token(SyntaxKind.CommaToken).AppendSpace(), .. separators];

        var @as = Argument(expression.WithoutLeadingTrivia());
        var newList = SeparatedList([@as, .. arguments], newSeparators);

        var newName = reducedFrom.Name;
        newName = isMemory ? GetNewName(reducedFrom) : RemoveAsync(newName);

        var fullyQualifiedName = $"{MakeType(reducedFrom.ContainingType)}.{newName}";

        var es = (ies.Expression is MemberAccessExpressionSyntax mae
            ? mae.ChangeIdentifier(fullyQualifiedName)
            : IdentifierName(Identifier(fullyQualifiedName)))
            .WithLeadingTrivia(expression.GetLeadingTrivia());

        return ies
            .WithExpression(es)
            .WithArgumentList(ArgumentList(newList));
    }

    private static string GetNewName(IMethodSymbol methodSymbol)
    {
        var containingType = methodSymbol.ContainingType;
        var replacement = ReplaceWithSpan(methodSymbol.Name);
        var newSymbol = containingType.GetMembers().FirstOrDefault(z => z.Name == replacement);
        return replacement;
    }

    private static bool IsTaskOrValueTask(INamedTypeSymbol symbol) => symbol is
    {
        Name: Task or ValueTask, IsGenericType: false,
        ContainingNamespace: { Name: Tasks, ContainingNamespace: { Name: Threading, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
    };

    private static bool IsValueTask(INamedTypeSymbol symbol) => symbol is
    {
        Name: ValueTask, IsGenericType: false,
        ContainingNamespace: { Name: Tasks, ContainingNamespace: { Name: Threading, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
    };

    private static bool IsSystemFunc(INamedTypeSymbol symbol) => symbol is
    {
        Name: Func, IsGenericType: true, TypeArguments: [{ }, ..],
        ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true }
    };

    private static bool IsCancellationToken(INamedTypeSymbol symbol) => symbol is
    {
        Name: CancellationToken, IsGenericType: false,
        ContainingNamespace: { Name: Threading, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } }
    };

    private static bool IsIProgress(INamedTypeSymbol symbol) => symbol is
    {
        Name: IProgress, IsGenericType: true,
        ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true }
    };

    private static TypeSyntax? ConvertFuncToAction(TypeSyntax originalType, INamedTypeSymbol namedTypeSymbol)
    {
        var typeArgs = namedTypeSymbol.TypeArguments;

        if (typeArgs[^1] is not INamedTypeSymbol lastTypeArgument
            || originalType is not GenericNameSyntax gns)
        {
            return null;
        }

        if (IsSystemFunc(lastTypeArgument))
        {
            if (ConvertFuncToAction(gns.TypeArgumentList.Arguments[^1], lastTypeArgument) is not { } child)
            {
                return null;
            }

            var newList = (List<TypeSyntax>)[.. gns.TypeArgumentList.Arguments];
            newList.RemoveAt(newList.Count - 1);
            newList.Add(child);
            var newSeparatedList = SeparatedList(newList, gns.TypeArgumentList.Arguments.GetSeparators());
            var res = gns.WithTypeArgumentList(TypeArgumentList(newSeparatedList));
            return res;
        }
        else if (!IsTaskOrValueTask(lastTypeArgument))
        {
            return null;
        }

        TypeSyntax newTypeSyntax;
        var newType = Global("System.Action");

        var list = new List<TypeSyntax>();
        if (typeArgs.Length > 1)
        {
            // Func<something, Task> => Action<something>
            for (var i = 0; i < typeArgs.Length - 1; i++)
            {
                list.Add(ProcessSymbol(typeArgs[i]));
            }

            var originalSeparators = (List<SyntaxToken>)[.. gns.TypeArgumentList.Arguments.GetSeparators()];

            if (originalSeparators.Count > 0 && list.Count == originalSeparators.Count)
            {
                originalSeparators.RemoveAt(originalSeparators.Count - 1);
            }

            var separatedList = SeparatedList(list, originalSeparators);
            newTypeSyntax = GenericName(Identifier(newType), TypeArgumentList(separatedList));
        }
        else
        {
            // Func<Task> => Action
            newTypeSyntax = IdentifierName(newType);
        }

        return newTypeSyntax;
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

    private TypeSyntax MaybeNullableType(TypeSyntax argumentTypeExpr, bool isValueType = false)
        => disableNullable && !isValueType ? argumentTypeExpr : NullableType(argumentTypeExpr);

    private void ProcessTrivia(SyntaxTriviaList syntaxTriviaList, DirectiveStack directiveStack, List<SyntaxTrivia> triviaList, Action<MemberDeclarationSyntax> process)
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

        // Remembers if the last #if was SYNC_ONLY
        var ifDirectives = directiveStack.Stack;

        foreach (var trivia in syntaxTriviaList)
        {
            if (trivia.IsKind(SyntaxKind.IfDirectiveTrivia) && trivia.GetStructure() is IfDirectiveTriviaSyntax ifDirective)
            {
                var syncOnlyDirectiveType = GetDirectiveType(ifDirective.Condition);

                if (syncOnlyDirectiveType == SyncOnlyDirectiveType.Invalid)
                {
                    var d = ReportedDiagnostic.Create(InvalidCondition, trivia.GetLocation(), trivia.ToString());
                    diagnostics.Add(d);
                    return;
                }

                if (syncOnlyDirectiveType != SyncOnlyDirectiveType.None)
                {
                    if (directiveStack.IsSyncOnly is { } isStackSyncOnly)
                    {
                        if (isStackSyncOnly ^ syncOnlyDirectiveType == SyncOnlyDirectiveType.SyncOnly)
                        {
                            var d = ReportedDiagnostic.Create(InvalidNesting, trivia.GetLocation(), trivia.ToString());
                            diagnostics.Add(d);
                            return;
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
                    return;
                }

                continue;
            }

            if (trivia.IsKind(SyntaxKind.DisabledTextTrivia) && directiveStack.IsSyncOnly == true)
            {
                var statementsText = trivia.ToString();
                var compilation = ParseCompilationUnit(statementsText);

                foreach (var m in compilation.Members)
                {
                    process(m);
                }

                continue;
            }

            triviaList.Add(trivia);
        }
    }

    private ExtraNodeInfo? ProcessTrivia(SyntaxTriviaList syntaxTriviaList, DirectiveStack directiveStack)
    {
        var statements = new List<StatementSyntax>();
        var triviaList = new List<SyntaxTrivia>();

        ProcessTrivia(syntaxTriviaList, directiveStack, triviaList, (m) =>
        {
            if (m is not GlobalStatementSyntax gs)
            {
                return;
            }

            var statement = gs.Statement;
            if (triviaList.Count > 0)
            {
                triviaList.AddRange([.. statement.GetLeadingTrivia()]);
                statement = statement.WithLeadingTrivia(triviaList);
                triviaList.Clear();
            }

            //Cannot to the following because syntax node is not within syntax tree
            //var statement = (StatementSyntax)Visit(gs.Statement)!;
            statements.Add(statement);
        });

        return new(false, List(statements), triviaList);
    }

    private SyncOnlyAttributeContext ProcessSyncOnlyAttributes(SyntaxTriviaList syntaxTriviaList, DirectiveStack directiveStack)
    {
        var attributes = new List<AttributeListSyntax>();
        var triviaList = new List<SyntaxTrivia>();

        ProcessTrivia(syntaxTriviaList, directiveStack, triviaList, (m) =>
        {
            if (m is not IncompleteMemberSyntax ims)
            {
                return;
            }

            var originalAttributes = ims.AttributeLists;
            if (triviaList.Count > 0)
            {
                ////triviaList.AddRange([.. statement.GetLeadingTrivia()]);
                ////statement = statement.WithLeadingTrivia(triviaList);
                ////triviaList.Clear();
            }

            //Cannot to the following because syntax node is not within syntax tree
            ////var statement = (StatementSyntax)Visit(gs.Statement)!;

            foreach (var o in originalAttributes)
            {
                attributes.Add(o);
            }
        });

        return new(List(attributes), triviaList);
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
        return typeSymbol is null ? typeSyntax : ProcessSymbol(typeSymbol).WithTriviaFrom(typeSyntax);
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
        ReturnStatementSyntax { Parent.Parent: MethodDeclarationSyntax, Expression: { } re } => ShouldRemoveArgument(re),
        _ => false,
    };

    private bool ChecksIfNegatedIsCancellationRequested(ExpressionSyntax condition)
        => RemoveParentheses(condition) is PrefixUnaryExpressionSyntax { OperatorToken.RawKind: (int)SyntaxKind.ExclamationToken } pe
        && RemoveParentheses(pe.Operand) is MemberAccessExpressionSyntax { Name.Identifier.ValueText: IsCancellationRequested } mae
        && GetSymbol(mae) is { } t
        && IsCancellationToken(t.ContainingType);

    private bool HasSymbolAndShouldBeRemoved(ExpressionSyntax expr, bool isNegated = false)
        => GetSymbol(expr) is ISymbol symbol && ShouldRemoveArgument(symbol, isNegated);

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

        if (symbol is IMethodSymbol methodSymbol
            && expression is MemberAccessExpressionSyntax memberAccessExpression
            && IsTaskExtension(methodSymbol) && memberAccessExpression.Expression is InvocationExpressionSyntax childInvocation)
        {
            return DropInvocation(childInvocation);
        }

        IParameterSymbol? GetParameter(ISymbol symbol, InvocationExpressionSyntax node) => symbol switch
        {
            IParameterSymbol ps => ps,
            IMethodSymbol { MethodKind: MethodKind.DelegateInvoke }
                => node.Expression switch
                {
                    MemberAccessExpressionSyntax { Expression: InvocationExpressionSyntax mae }
                        when GetSymbol(mae.Expression) is { } parentSymbol
                        => GetParameter(parentSymbol, mae),
                    MemberAccessExpressionSyntax mae
                        when GetSymbol(mae.Expression) is IParameterSymbol ps
                        => ps,
                    InvocationExpressionSyntax parentIes
                        when GetSymbol(parentIes.Expression) is { } parentSymbol
                        => GetParameter(parentSymbol, parentIes),
                    _ => null,
                },
            _ => null,
        };

        // Ensure that if a parameter is called, which hasn't been removed, invocation isn't dropped.
        return (GetParameter(symbol, invocation) is not IParameterSymbol ps || removedParameters.Contains(ps)) && HasSymbolAndShouldBeRemoved(invocation);
    }

    private bool ShouldRemoveArrowExpression(ArrowExpressionClauseSyntax? arrowNullable)
        => arrowNullable is { } arrow && ShouldRemoveArgument(arrow.Expression);

    [System.Diagnostics.CodeAnalysis.SuppressMessage("Style", "IDE0072:Add missing cases", Justification = "https://github.com/dotnet/roslyn/issues/50983")]
    private StatementSyntax? ExpressionToStatement(ExpressionSyntax result)
    {
        // Conditional expression to if statement
        if (result is ConditionalExpressionSyntax conditionalExpression)
        {
            var condition = conditionalExpression.Condition.WithoutTrailingTrivia();

            var syntax = (ExpressionToStatement(conditionalExpression.WhenTrue), ExpressionToStatement(conditionalExpression.WhenFalse)) switch
            {
                (null, null) => null,
                (null, { } elseStatement) => IfStatement(PrefixUnaryExpression(SyntaxKind.LogicalNotExpression, condition), elseStatement),
                ({ } statement, null) => IfStatement(condition, statement),
                ({ } statement, { } elseStatement) => IfStatement(condition, statement, ElseClause(elseStatement).WithElseKeyword(Token(SyntaxKind.ElseKeyword).PrependSpace().AppendSpace())),
            };

            return syntax?
                .WithIfKeyword(syntax.IfKeyword.AppendSpace())
                .WithCloseParenToken(syntax.CloseParenToken.AppendSpace());
        }

        return ShouldRemoveArgument(result) ? null
            : (StatementSyntax)ExpressionStatement((ExpressionSyntax)Visit(result).WithoutTrivia());
    }

    private bool ShouldRemoveArgument(ExpressionSyntax expr)
        => ShouldRemoveArgument(expr, new());

    private bool ShouldRemoveArgument(ExpressionSyntax expr, RemoveArgumentContext context) => expr switch
    {
        ElementAccessExpressionSyntax ee => ShouldRemoveArgument(ee.Expression, context),
        BinaryExpressionSyntax be => ShouldRemoveArgument(be.Left, context) || ShouldRemoveArgument(be.Right, context),
        CastExpressionSyntax ce => HasSymbolAndShouldBeRemoved(expr) || ShouldRemoveArgument(ce.Expression, context),
        ParenthesizedExpressionSyntax pe => ShouldRemoveArgument(pe.Expression, context),
        IdentifierNameSyntax id => !id.Identifier.ValueText.EndsWithAsync() && HasSymbolAndShouldBeRemoved(id, context.IsNegated),
        InvocationExpressionSyntax ie => DropInvocation(ie),
        ConditionalExpressionSyntax ce => ShouldRemoveArgument(ce.WhenTrue, context) && ShouldRemoveArgument(ce.WhenFalse, context),
        MemberAccessExpressionSyntax mae => ShouldRemoveArgument(mae.Name, context),
        PostfixUnaryExpressionSyntax pue => ShouldRemoveArgument(pue.Operand, context),
        PrefixUnaryExpressionSyntax pue => ProcessUnaryExpression(pue, context),
        ObjectCreationExpressionSyntax oe => ShouldRemoveArgument(oe.Type, context) || ShouldRemoveObjectCreation(oe),
        ImplicitObjectCreationExpressionSyntax oe => ShouldRemoveObjectCreation(oe),
        ConditionalAccessExpressionSyntax cae => ShouldRemoveArgument(cae.Expression, context),
        AwaitExpressionSyntax ae => ShouldRemoveArgument(ae.Expression, context),
        AssignmentExpressionSyntax ae => ShouldRemoveArgument(ae.Right, context),
        GenericNameSyntax gn => HasSymbolAndShouldBeRemoved(gn),
        LiteralExpressionSyntax le => ShouldRemoveLiteral(le),
        _ => false,
    };

    private bool ProcessUnaryExpression(PrefixUnaryExpressionSyntax pue, RemoveArgumentContext context)
    {
        if (pue.OperatorToken.IsKind(SyntaxKind.ExclamationToken))
        {
            context = context with { IsNegated = !context.IsNegated };
        }

        return ShouldRemoveArgument(pue.Operand, context);
    }

    private bool ShouldRemoveLiteral(LiteralExpressionSyntax literalExpression)
        => literalExpression.Token.IsKind(SyntaxKind.DefaultKeyword)
           && semanticModel.GetTypeInfo(literalExpression).Type is INamedTypeSymbol named && IsValueTask(named);

    private bool ShouldRemoveObjectCreation(BaseObjectCreationExpressionSyntax oe)
        => GetSymbol(oe) is IMethodSymbol { ReceiverType: INamedTypeSymbol named } && IsValueTask(named);

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

    private sealed record SyncOnlyAttributeContext(SyntaxList<AttributeListSyntax> Attributes, IList<SyntaxTrivia> LeadingTrivia);

    private sealed record ExtraNodeInfo(bool DropOriginal, SyntaxList<StatementSyntax> AdditionalStatements, IList<SyntaxTrivia> LeadingTrivia)
    {
        public ExtraNodeInfo(bool dropOriginal)
            : this(dropOriginal, List(Array.Empty<StatementSyntax>()), Array.Empty<SyntaxTrivia>())
        {
        }

        public static implicit operator ExtraNodeInfo(bool b) => new(b);
    }

    private sealed class StatementProcessor
    {
        private readonly Dictionary<int, ExtraNodeInfo> extraNodeInfoList = [];

        public StatementProcessor(AsyncToSyncRewriter rewriter, SyntaxList<StatementSyntax> statements, DirectiveStack directiveStack)
        {
            HasErrors = !rewriter.PreProcess(statements, extraNodeInfoList, directiveStack);
        }

        public bool HasErrors { get; }

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

    private sealed record ExtensionExprSymbol(InvocationExpressionSyntax InvocationExpression, ITypeSymbol ReturnType);

    private sealed record RemoveArgumentContext(bool IsNegated = false);
}
