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
    private const string ReadOnlyMemory = "System.ReadOnlyMemory";
    private const string Memory = "System.Memory";
    private const string TaskType = "System.Threading.Tasks.Task";
    private const string ValueTaskType = "System.Threading.Tasks.ValueTask";
    private const string IProgressInterface = "System.IProgress";
    private const string IAsyncResultInterface = "System.IAsyncResult";
    private const string CancellationTokenType = "System.Threading.CancellationToken";
    private static readonly HashSet<string> Drops = new(new[] { IProgressInterface, CancellationTokenType });
    private static readonly HashSet<string> InterfacesToDrop = new(new[] { IProgressInterface, IAsyncResultInterface });
    private static readonly Dictionary<string, string?> Replacements = new()
    {
        { "System.Collections.Generic.IAsyncEnumerable", "System.Collections.Generic.IEnumerable" },
        { "System.Collections.Generic.IAsyncEnumerator", "System.Collections.Generic.IEnumerator" },
        { TaskType, null },
        { ValueTaskType, null },
        { ReadOnlyMemory, "System.ReadOnlySpan" },
        { Memory, "System.Span" },
    };

    private readonly SemanticModel semanticModel = semanticModel;
    private readonly HashSet<string> removedParameters = new();
    private readonly HashSet<string> memoryToSpan = new();
    private readonly Dictionary<string, string> renamedLocalFunctions = new();
    private readonly ImmutableArray<Diagnostic>.Builder diagnostics = ImmutableArray.CreateBuilder<Diagnostic>();

    /// <summary>
    /// Gets the diagnostics messages.
    /// </summary>
    public ImmutableArray<Diagnostic> Diagnostics => diagnostics.ToImmutable();

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

        static string? Convert(string key)
            => Replacements.TryGetValue(key, out var replacement) ? replacement : key;

        if (symbol is not INamedTypeSymbol)
        {
            return @base;
        }

        var genericName = GetNameWithoutTypeParams(symbol);

        var newType = Convert(genericName);

        if (node.Parent is ParameterSyntax ps && genericName is ReadOnlyMemory or Memory)
        {
            memoryToSpan.Add(ps.Identifier.ValueText);
        }

        if (newType is not null)
        {
            return @base.WithIdentifier(SyntaxFactory.Identifier("global::" + newType))
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

        return @base;
    }

    public override SyntaxNode? VisitLocalFunctionStatement(LocalFunctionStatementSyntax node)
    {
        var @base = (LocalFunctionStatementSyntax)base.VisitLocalFunctionStatement(node)!;

        var newNode = @base;

        var identifier = @base.Identifier;
        if (identifier.ValueText.EndsWithAsync())
        {
            var newName = RemoveAsync(identifier.ValueText);
            renamedLocalFunctions.Add(identifier.ValueText, newName);
            newNode = @base.WithIdentifier(SyntaxFactory.Identifier(newName));
        }

        return newNode
            .WithModifiers(StripAsyncModifier(@base.Modifiers))
            .WithTriviaFrom(@base);
    }

    public override SyntaxNode? VisitParameterList(ParameterListSyntax node)
    {
        var newNode = (ParameterListSyntax)base.VisitParameterList(node)!;
        bool IsValidParameter(ParameterSyntax ps)
        {
            if (ps.Type is not { } type1)
            {
                return true;
            }

            if (type1 is NullableTypeSyntax nuts)
            {
                type1 = nuts.ElementType;
            }

            if (semanticModel.GetTypeInfo(type1).Type is not INamedTypeSymbol nts)
            {
                return true;
            }

            if (ShouldRemoveType(nts))
            {
                removedParameters.Add(ps.Identifier.ValueText);
                return false;
            }

            return true;
        }

        var invalid = node.Parameters.GetIndices(v => !IsValidParameter(v));
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

        var symbol = GetSymbol(node.Expression);
        if (symbol is ITypeSymbol && node.Expression is TypeSyntax type)
        {
            // Rewrite static invocation (eg. File.ReadAllTextAsync)
            var newType = ProcessType(type);
            if (newType != type)
            {
                @base = @base.WithExpression(newType);
            }
        }

        if (@base.Name.Identifier.ValueText == "Span"
            && @base.Expression is IdentifierNameSyntax ins && memoryToSpan.Contains(ins.Identifier.ValueText))
        {
            return @base.Expression;
        }
        else if (@base.Name.Identifier.ValueText.EndsWithAsync())
        {
            return @base.WithName(SyntaxFactory.IdentifierName(RemoveAsync(@base.Name.Identifier.ValueText)));
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

            var siblings = methodSymbol.ContainingType.GetMembers().Where(z => z is IMethodSymbol).ToList();
            var hasSync = siblings.Any(z => z.Name == newName);
            var hasAsyncWithAttr = siblings.Any(z => z.Name == ins.Identifier.ValueText
                && z.GetAttributes().Any(z
                    => z.AttributeClass is not null && IsCreateSyncVersionAttribute(z.AttributeClass)));

            if (string.IsNullOrWhiteSpace(newName))
            {
                // Should Return diagnostics message
                return @base;
            }

            return @base.WithExpression(SyntaxFactory.IdentifierName(newName));
        }

        var returnType = methodSymbol.ReturnType;
        var typeString = GetNameWithoutTypeParams(returnType);
        var isMemory = typeString is ReadOnlyMemory or Memory;

        string? GetNewName(string name)
        {
            var containingType = methodSymbol.ContainingType;
            var replacement = Regex.Replace(name, "Memory", "Span");
            var newSymbol = containingType.GetMembers().FirstOrDefault(z => z.Name == replacement);
            if (newSymbol is null)
            {
                return null;
            }

            return replacement;
        }

        if (methodSymbol is { IsExtensionMethod: true, ReducedFrom: { } reducedFrom }
            && node.Expression is MemberAccessExpressionSyntax zz)
        {
            var arguments = @base.ArgumentList.Arguments;
            var separators = arguments.GetSeparators();

            var newSeparators = arguments.Count < 1 ? Array.Empty<SyntaxToken>()
                : new[] { SyntaxFactory.Token(SyntaxKind.CommaToken).WithTrailingTrivia(SyntaxFactory.Space) }
                .Union(separators);

            ArgumentSyntax @as;
            if (@base.Expression is MemberAccessExpressionSyntax memberAccess2)
            {
                @as = SyntaxFactory.Argument(memberAccess2.Expression);
            }
            else
            {
                throw new InvalidOperationException($"Need to handle {node.Expression}");
            }

            var newList = SyntaxFactory.SeparatedList(new[] { @as }.Union(arguments), newSeparators);

            newName = reducedFrom.Name;
            if (isMemory)
            {
                newName = GetNewName(symbol.Name);
            }
            else if (symbol is { Name: "WithCancellation", ContainingNamespace.Name: "Tasks" })
            {
                return zz.Expression;
            }
            else
            {
                newName = RemoveAsync(newName);
            }

            var id = SyntaxFactory.Identifier($"{MakeType(reducedFrom.ContainingType)}.{newName}");

            ExpressionSyntax es = @base.Expression is MemberAccessExpressionSyntax { Name: GenericNameSyntax gns }
                ? SyntaxFactory.GenericName(id, gns.TypeArgumentList)
                : SyntaxFactory.IdentifierName(id);

            return @base
                .WithExpression(es)
                .WithArgumentList(SyntaxFactory.ArgumentList(newList));
        }

        if (isMemory)
        {
            newName = GetNewName(symbol.Name);
        }

        if (@base.Expression is not MemberAccessExpressionSyntax memberAccess
            || memberAccess.Name is not IdentifierNameSyntax mins)
        {
            return @base;
        }

        if (memberAccess.Name is IdentifierNameSyntax { Identifier.ValueText: "ConfigureAwait" })
        {
            return memberAccess.Expression;
        }

        if (mins.Identifier.ValueText.EndsWithAsync() || mins.Identifier.ValueText == "GetAsyncEnumerator")
        {
            newName = RemoveAsync(mins.Identifier.ValueText);
        }
        else if (mins.Identifier.ValueText == "FromResult" && @base.ArgumentList.Arguments.Count > 0)
        {
            return @base.ArgumentList.Arguments[0].Expression;
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
        var dropParentheses = node.Expression is AwaitExpressionSyntax;
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

    public override SyntaxNode? VisitSwitchSection(SwitchSectionSyntax node)
    {
        var indices = DropStatements(node.Statements);

        var @base = (SwitchSectionSyntax)base.VisitSwitchSection(node)!;

        if (indices.Count > 0)
        {
            var newStatements = RemoveAtRange(@base.Statements, indices);
            @base = @base.WithStatements(newStatements);
        }

        return @base;
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
    public override SyntaxNode? VisitBlock(BlockSyntax node)
    {
        var indices = DropStatements(node.Statements);

        var @base = (BlockSyntax)base.VisitBlock(node)!;

        var i = 0;
        foreach (var statement in @base.Statements)
        {
            if (CanDropEmptyStatement(statement))
            {
                indices.Add(i);
            }

            ++i;
        }

        var newStatements = RemoveAtRange(@base.Statements, indices);
        return @base.WithStatements(newStatements);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitTypeArgumentList(TypeArgumentListSyntax node)
    {
        var @base = (TypeArgumentListSyntax)base.VisitTypeArgumentList(node)!;
        return @base.WithArguments(SyntaxFactory.SeparatedList(
            @base.Arguments.Select(z => z.SyntaxTree == node.SyntaxTree ? ProcessType(z) : z),
            @base.Arguments.GetSeparators()));
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
        var attributes = node.AttributeLists.Select(
            z => SyntaxFactory.AttributeList(SyntaxFactory.SeparatedList(z.Attributes.Where(ShouldPreserveAttribute)))
            .WithTriviaFrom(z));

        var newAttributes = SyntaxFactory.List(attributes.Where(z => z.Attributes.Any()));

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

        bool ShouldPreserveAttribute(AttributeSyntax attributeSyntax)
        {
            if (GetSymbol(attributeSyntax) is not IMethodSymbol attributeSymbol)
            {
                return false;
            }

            var attributeContainingTypeSymbol = attributeSymbol.ContainingType;

            // Is the attribute [CreateSyncVersion] attribute?
            return !IsCreateSyncVersionAttribute(attributeContainingTypeSymbol);
        }

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
                    if (attribute is not null)
                    {
                        if (removedParameters.Contains(attribute.Identifier.Identifier.ValueText))
                        {
                            indicesToRemove.Add(i - 1); // preceding slashes
                            indicesToRemove.Add(i);
                        }
                    }
                }

                ++i;
            }

            var newContent = RemoveAtRange(comment.Content, indicesToRemove);
            comment = comment.WithContent(newContent);
            var newTrivia = SyntaxFactory.Trivia(comment);
            newTriviaList = trivia.Replace(comments, newTrivia);
        }

        bool Preprocessors(SyntaxTrivia st)
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

        var newReturnType = isTask
            ? SyntaxFactory.IdentifierName("void")
                .WithTriviaFrom(returnType)
            : @base.ReturnType;

        var retVal = @base
            .WithIdentifier(SyntaxFactory.Identifier(newName))
            .WithReturnType(newReturnType)
            .WithModifiers(StripAsyncModifier(@base.Modifiers))
            .WithAttributeLists(newAttributes)
            .WithLeadingTrivia(newTriviaList)
            ;

        return retVal;
    }

    public override SyntaxNode? VisitArgumentList(ArgumentListSyntax node)
    {
        var @base = (ArgumentListSyntax)base.VisitArgumentList(node)!;
        var invalid = node.Arguments.GetIndices(v => ShouldRemoveArgument(v.Expression));

        var newList = RemoveAtRange(@base.Arguments, invalid);
        return @base.WithArguments(newList);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitForEachStatement(ForEachStatementSyntax node)
    {
        var @base = (ForEachStatementSyntax)base.VisitForEachStatement(node)!;
        return @base.WithAwaitKeyword(default).WithType(ProcessType(node.Type)).WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
    {
        var @base = (LocalDeclarationStatementSyntax)base.VisitLocalDeclarationStatement(node)!;

        var variableTypeName = node.Declaration.Type;

        var variableType = semanticModel
            .GetSymbolInfo(variableTypeName)
            .Symbol;

        if (variableType is INamedTypeSymbol { IsGenericType: true } n)
        {
            var name = GetNameWithoutTypeParams(n);
            if (name is ReadOnlyMemory or Memory)
            {
                foreach (var variable in node.Declaration.Variables)
                {
                    memoryToSpan.Add(variable.Identifier.Text);
                }
            }
        }

        return @base.WithAwaitKeyword(default).WithTriviaFrom(@base);
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

    private static string GetNameWithoutTypeParams(ISymbol symbol)
        => symbol.ContainingNamespace + "." + symbol.Name;

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
        => s.ToDisplayString() == SyncMethodSourceGenerator.CreateSyncVersionAttribute;

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

    private static bool ShouldRemoveArgument(ISymbol symbol) => symbol switch
    {
        IFieldSymbol fs => ShouldRemoveType(fs.Type),
        IPropertySymbol ps => ShouldRemoveType(ps.Type),
        ITypeSymbol ts => ShouldRemoveType(ts),
        ILocalSymbol ls => ShouldRemoveType(ls.Type),
        IParameterSymbol ps => ShouldRemoveType(ps.Type),
        IMethodSymbol ms => ShouldRemoveType(ms.ReturnType)
            || (ms.ReceiverType is { } receiver && ShouldRemoveType(receiver)),
        _ => false,
    };

    private static bool CanDropEmptyStatement(StatementSyntax statement)
        => statement switch
        {
            IfStatementSyntax @if => CanDropIf(@if),
            SwitchStatementSyntax s => CanDropSwitch(s),
            _ => false,
        };

    private ISymbol? GetSymbol(SyntaxNode node) => semanticModel.GetSymbolInfo(node).Symbol;

    private bool CanDropDeclaration(LocalDeclarationStatementSyntax local)
    {
        var symbol = GetSymbol(local.Declaration.Type);
        return symbol is not null && ShouldRemoveArgument(symbol);
    }

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
        _ => false,
    };

    private HashSet<int> DropStatements(SyntaxList<StatementSyntax> statements)
    {
        var i = 0;
        var indices = new HashSet<int>();

        foreach (var statement in statements)
        {
            if (CanDropStatement(statement))
            {
                indices.Add(i);
            }

            ++i;
        }

        return indices;
    }

    private bool HasSymbolAndShouldBeRemoved(ExpressionSyntax expr)
        => GetSymbol(expr) is ISymbol symbol && ShouldRemoveArgument(symbol);

    private bool EndsWithAsync(ExpressionSyntax expression) => expression switch
    {
        IdentifierNameSyntax id => id.Identifier.ValueText.EndsWithAsync(),
        MemberAccessExpressionSyntax m => EndsWithAsync(m.Name),
        _ => false,
    };

    private bool DropInvocation(InvocationExpressionSyntax invocation)
    {
        if (EndsWithAsync(invocation.Expression))
        {
            return false;
        }

        return HasSymbolAndShouldBeRemoved(invocation);
    }

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
        _ => false,
    };
}
