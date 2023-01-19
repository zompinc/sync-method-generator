namespace Zomp.SyncMethodGenerator;

/// <summary>
/// Rewrites a method synchronously
/// </summary>
internal class AsyncToSyncRewriter : CSharpSyntaxRewriter
{
    private readonly SemanticModel semanticModel;
    readonly HashSet<string> removedParameters = new();
    readonly HashSet<string> memoryToSpan = new();
    readonly Dictionary<string, string> renamedLocalFunctions = new();
    readonly ImmutableArray<Diagnostic>.Builder diagnostics = ImmutableArray.CreateBuilder<Diagnostic>();

    const string ReadOnlyMemory = "System.ReadOnlyMemory";
    const string Memory = "System.Memory";
    const string TaskType = "System.Threading.Tasks.Task";
    const string ValueTaskType = "System.Threading.Tasks.ValueTask";

    private static readonly Dictionary<string, string?> Replacements = new()
    {
        { "System.Collections.Generic.IAsyncEnumerable", "System.Collections.Generic.IEnumerable" },
        { "System.Collections.Generic.IAsyncEnumerator", "System.Collections.Generic.IEnumerator" },
        { TaskType, null},
        { ValueTaskType, null},
        { ReadOnlyMemory, "System.ReadOnlySpan"},
        { Memory, "System.Span"},
    };

    /// <summary>
    /// Diagnostics
    /// </summary>
    public ImmutableArray<Diagnostic> Diagnostics => diagnostics.ToImmutable();

    /// <summary>
    /// Creates a new instance of <see cref="AsyncToSyncRewriter"/>.
    /// </summary>
    /// <param name="semanticModel"></param>
    public AsyncToSyncRewriter(SemanticModel semanticModel)
    {
        this.semanticModel = semanticModel;
    }

    static string MakeType(ISymbol symbol)
        => symbol switch
        {
            INamedTypeSymbol => "global::" + symbol.ToDisplayString(),
            _ => symbol.Name,
        };

    static TypeSyntax ProcessSymbol(ITypeSymbol typeSymbol) => typeSymbol switch
    {
        INamedTypeSymbol nts => SyntaxFactory.IdentifierName(MakeType(nts)),
        IArrayTypeSymbol ats => SyntaxFactory.IdentifierName(MakeType(ats.ElementType) + "[]"),
        _ => SyntaxFactory.IdentifierName(typeSymbol.Name),
    };

    TypeSyntax ProcessSyntaxUsingSymbol(TypeSyntax typeSyntax)
    {
        try
        {
            return (semanticModel.GetTypeInfo(typeSyntax).Type is { } symbol ? ProcessSymbol(symbol) : typeSyntax).WithTriviaFrom(typeSyntax);
        }
        catch (ArgumentException ex)
        {
            var message = Regex.Replace(ex.ToString(), "[\r\n]+", "", RegexOptions.Multiline);
            var diag = Diagnostic.Create(CannotRetrieveSymbol, typeSyntax.GetLocation(), typeSyntax.ToString(), message);
            diagnostics.Add(diag);
            return typeSyntax;
        }
    }

    private TypeSyntax ProcessType(TypeSyntax typeSyntax) => typeSyntax switch
    {
        IdentifierNameSyntax { Identifier.ValueText: "var" } => typeSyntax,
        IdentifierNameSyntax or QualifiedNameSyntax => ProcessSyntaxUsingSymbol(typeSyntax),
        _ => typeSyntax,

    };

    /// <inheritdoc/>
    public override SyntaxNode? VisitNullableType(NullableTypeSyntax node)
    {
        var @base = (NullableTypeSyntax)base.VisitNullableType(node)!;
        return @base.WithElementType(ProcessType(@base.ElementType)).WithTriviaFrom(@base);
    }

    static string GetNameWithoutTypeParams(ISymbol symbol)
        => symbol.ContainingNamespace + "." + symbol.Name;

    internal static bool IsTypeOfInterest(ITypeSymbol symbol)
    {
        var genericName = GetNameWithoutTypeParams(symbol);
        return Replacements.ContainsKey(genericName);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitGenericName(GenericNameSyntax node)
    {
        var @base = (GenericNameSyntax)base.VisitGenericName(node)!;

        var symbol = semanticModel.GetSymbolInfo(node).Symbol;

        static string? convert(string key)
            => Replacements.TryGetValue(key, out var replacement) ? replacement : key;

        if (symbol is not INamedTypeSymbol)
        {
            return @base;
        }

        var genericName = GetNameWithoutTypeParams(symbol);

        var newType = convert(genericName);

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

    static SyntaxTokenList StripAsyncModifier(SyntaxTokenList list)
        => SyntaxFactory.TokenList(list.Where(z => !z.IsKind(SyntaxKind.AsyncKeyword)));

    /// <inheritdoc/>
    public override SyntaxNode? VisitLocalFunctionStatement(LocalFunctionStatementSyntax node)
    {
        var @base = (LocalFunctionStatementSyntax)base.VisitLocalFunctionStatement(node)!;

        var newNode = @base;

        var identifier = @base.Identifier;
        if (identifier.ValueText.EndsWith("Async", StringComparison.OrdinalIgnoreCase))
        {
            var newName = RemoveAsync(identifier.ValueText);
            renamedLocalFunctions.Add(identifier.ValueText, newName);
            newNode = @base.WithIdentifier(SyntaxFactory.Identifier(newName));
        }

        return newNode
            .WithModifiers(StripAsyncModifier(@base.Modifiers))
            .WithTriviaFrom(@base);
    }

    static string RemoveAsync(string original)
        => Regex.Replace(original, "Async", string.Empty);

    /// <inheritdoc/>
    public override SyntaxNode? VisitParameterList(ParameterListSyntax node)
    {
        var newnode = (ParameterListSyntax)base.VisitParameterList(node)!;
        bool isValidParameter(ParameterSyntax ps)
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

            if (nts.Name is "CancellationToken" or "IProgress")
            {
                removedParameters.Add(ps.Identifier.ValueText);
                return false;
            }

            return true;
        }

        var invalid = node.Parameters
            .Select((ps, i) => new { ps, i })
            .Where(v => !isValidParameter(v.ps))
            .Select(t => t.i);

        var newParams = RemoveAtRange(newnode.Parameters, invalid);
        return newnode.WithParameters(newParams);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitParameter(ParameterSyntax node)
    {
        var @base = (ParameterSyntax)base.VisitParameter(node)!;
        if (node.Type is null or NullableTypeSyntax or GenericNameSyntax)
            return @base;
        return @base.WithType(ProcessType(node.Type)).WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
    {
        var @base = (MemberAccessExpressionSyntax)base.VisitMemberAccessExpression(node)!;

        var symbol = semanticModel.GetSymbolInfo(node.Expression).Symbol;
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
        else if (@base.Name.Identifier.ValueText.EndsWith("Async", StringComparison.OrdinalIgnoreCase))
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
        var symbol = semanticModel.GetSymbolInfo(node).Symbol;

        if (symbol is null)
        {
            if (@base.Expression is not IdentifierNameSyntax { Identifier.ValueText: "nameof" })
            {
                var diag = Diagnostic.Create(CannotRetrieveSymbol, node.GetLocation(), node.ToString(), $"Could not get symbol out of {node}");
                diagnostics.Add(diag);
            }
            return @base;
        }

        if (@base.Expression is IdentifierNameSyntax ins && ins.Identifier.ValueText.EndsWith("Async", StringComparison.OrdinalIgnoreCase))
        {
            if (symbol is not IMethodSymbol methodSymbol)
                throw new InvalidOperationException($"Could not get symbol of {node}");

            newName = RemoveAsync(ins.Identifier.ValueText);

            var siblings = methodSymbol.ContainingType.GetMembers().Where(z => z is IMethodSymbol).ToList();
            var hasSync = siblings.Any(z => z.Name == newName);
            var hasAsyncWithAttr = siblings.Any(z => z.Name == ins.Identifier.ValueText
                && z.GetAttributes().Any(z
                    => z.AttributeClass is not null && IsCreateSyncVerionAttribute(z.AttributeClass)));

            //if (symbol.con)

            if (string.IsNullOrWhiteSpace(newName))
            {
                // Should Return diagnistics message
                return @base;
            }
            return @base.WithExpression(SyntaxFactory.IdentifierName(newName));
        }

        if (symbol is IMethodSymbol { IsExtensionMethod: true, ReducedFrom: { } r }
            && node.Expression is MemberAccessExpressionSyntax zz)
        {
            var arguments = @base.ArgumentList.Arguments;
            var separators = arguments.GetSeparators();

            var newSeparators = arguments.Count < 1 ? Array.Empty<SyntaxToken>()
                : new[] { SyntaxFactory.Token(SyntaxKind.CommaToken).WithTrailingTrivia(SyntaxFactory.Space) }
                .Union(separators);

            ArgumentSyntax @as;
            if (node.Expression is MemberAccessExpressionSyntax maes2)
            {
                @as = SyntaxFactory.Argument(maes2.Expression);
            }
            else
            {
                throw new InvalidOperationException($"Need to handle {node.Expression}");
            }

            var newList = SyntaxFactory.SeparatedList(new[] { @as }.Union(arguments), newSeparators);

            newName = r.Name;
            if (symbol is { Name: "AsMemory", ContainingNamespace.Name: "System" })
            {
                newName = "AsSpan";
            }
            else if (symbol is { Name: "WithCancellation", ContainingNamespace.Name: "Tasks" })
            {
                return zz.Expression;
            }
            else
            {
                newName = RemoveAsync(newName);
            }

            var id = SyntaxFactory.Identifier($"{MakeType(r.ContainingType)}.{newName}");

            ExpressionSyntax es = @base.Expression is MemberAccessExpressionSyntax { Name: GenericNameSyntax gns }
                ? SyntaxFactory.GenericName(id, gns.TypeArgumentList)
                : SyntaxFactory.IdentifierName(id);

            return @base
                .WithExpression(es)
                .WithArgumentList(SyntaxFactory.ArgumentList(newList));
        }

        if (@base.Expression is not MemberAccessExpressionSyntax maes
            || maes.Name is not IdentifierNameSyntax mins)
        {
            return @base;
        }

        if (maes.Name is IdentifierNameSyntax { Identifier.ValueText: "WithCancellation" or "ConfigureAwait" })
        {
            return maes.Expression;
        }

        if (mins.Identifier.ValueText.EndsWith("Async", StringComparison.OrdinalIgnoreCase) || mins.Identifier.ValueText == "GetAsyncEnumerator")
        {
            newName = RemoveAsync(mins.Identifier.ValueText);
        }
        else if (mins.Identifier.ValueText == "FromResult" && @base.ArgumentList.Arguments.Count > 0)
        {
            return @base.ArgumentList.Arguments[0].Expression;
        }

        if (newName == null)
            return @base;
        return @base.WithExpression(maes.WithName(SyntaxFactory.IdentifierName(newName)));
    }

    bool NeedToDrop(ExpressionSyntax expr)
    {
        if (expr is IdentifierNameSyntax ins)
        {
            var result = removedParameters.Contains(ins.Identifier.ValueText);
            return result;
        }

        if (expr is ConditionalAccessExpressionSyntax cae)
        {
            return NeedToDrop(cae.Expression);
        }

        if (expr is InvocationExpressionSyntax ies)
        {
            return NeedToDrop(ies.Expression);
        }
        if (expr is MemberAccessExpressionSyntax mes)
        {
            return NeedToDrop(mes.Expression);
        }
        return false;
    }

    static bool CanDropIf(IfStatementSyntax ifst)
    {
        return ifst.Statement is BlockSyntax { Statements.Count: 0 } or null
            && ifst.Condition is BinaryExpressionSyntax
            && ifst.Else is null;
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
        var symbol = semanticModel.GetSymbolInfo(node).Symbol;
        if (symbol is null
            or { ContainingType.IsGenericType: true }
            or INamedTypeSymbol { IsGenericType: true })
            return @base;

        var newType = ProcessSymbol(symbol.ContainingType);
        return newType == node.Type ? @base : @base.WithType(newType);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitBlock(BlockSyntax node)
    {
        var @base = (BlockSyntax)base.VisitBlock(node)!;
        var i = 0;
        var indicies = new List<int>();
        foreach (var statement in @base.Statements)
        {
            if (statement is ExpressionStatementSyntax ess)
            {
                if (NeedToDrop(ess.Expression))
                {
                    indicies.Add(i);
                }
            }
            else if (statement is IfStatementSyntax @if)
            {
                if (CanDropIf(@if))
                {
                    indicies.Add(i);
                }
            }
            ++i;
        }

        var newStatements = RemoveAtRange(@base.Statements, indicies);
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
            return @base;

        return @base.WithType(newType).WithTriviaFrom(@base);
    }

    static bool IsCreateSyncVerionAttribute(INamedTypeSymbol s)
        => s.ToDisplayString() == SyncMethodSourceGenerator.CreateSyncVersionAttribute;

    /// <inheritdoc/>
    public override SyntaxNode? VisitMethodDeclaration(MethodDeclarationSyntax node)
    {
        var attrinutes = node.AttributeLists.Select(
            z => SyntaxFactory.AttributeList(SyntaxFactory.SeparatedList(z.Attributes.Where(ShouldPreserveAttribute)))
            .WithTriviaFrom(z)
            );

        var newAttributes = SyntaxFactory.List(attrinutes.Where(z => z.Attributes.Any()));

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
            if (semanticModel.GetSymbolInfo(attributeSyntax).Symbol is not IMethodSymbol attributeSymbol)
            {
                return false;
            }

            var attributeContainingTypeSymbol = attributeSymbol.ContainingType;
            // Is the attribute [CreateSyncVersion] attribute?
            return !IsCreateSyncVerionAttribute(attributeContainingTypeSymbol);
        }

        // documenetation

        var trivia = node.GetLeadingTrivia();
        var newTriviaList = trivia;
        var comments = trivia.FirstOrDefault(t => t.IsKind(SyntaxKind.SingleLineDocumentationCommentTrivia));
        var xml = comments.GetStructure();

        DocumentationCommentTriviaSyntax? dcts = null;
        if (xml is DocumentationCommentTriviaSyntax syntax)
        {
            dcts = syntax;
            var i = 0;
            var indicesToRemove = new List<int>();

            foreach (var commentLine in dcts.Content)
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

            var newContent = RemoveAtRange(dcts.Content, indicesToRemove);
            dcts = dcts.WithContent(newContent);
            var newTrivia = SyntaxFactory.Trivia(dcts);
            newTriviaList = trivia.Replace(comments, newTrivia);
        }

        bool preprocessors(SyntaxTrivia st)
        {
            return st.IsKind(SyntaxKind.IfDirectiveTrivia)
                || st.IsKind(SyntaxKind.ElifDirectiveTrivia)
                || st.IsKind(SyntaxKind.ElseDirectiveTrivia)
                || st.IsKind(SyntaxKind.EndIfDirectiveTrivia)
                || st.IsKind(SyntaxKind.RegionDirectiveTrivia)
                || st.IsKind(SyntaxKind.EndRegionDirectiveTrivia)
                || st.IsKind(SyntaxKind.DisabledTextTrivia);
        }

        var z = newTriviaList.Where(preprocessors).ToList();

        while (newTriviaList.FirstOrDefault(preprocessors) is { } preprocessor
            && preprocessor != default)
        {
            newTriviaList = newTriviaList.Remove(preprocessor);
        }

        var newReturnType = isTask
            ? SyntaxFactory.IdentifierName("void")
                .WithTriviaFrom(returnType)
            : @base.ReturnType;

        var retval = @base
            .WithIdentifier(SyntaxFactory.Identifier(newName))
            .WithReturnType(newReturnType)
            .WithModifiers(StripAsyncModifier(@base.Modifiers))
            .WithAttributeLists(newAttributes)
            .WithLeadingTrivia(newTriviaList)
            ;

        return retval;
    }

    /// <inheritdoc/>
    public static SyntaxList<TNode> RemoveAtRange<TNode>(SyntaxList<TNode> list, IEnumerable<int> indices) where TNode : SyntaxNode
    {
        var newContent = list;

        foreach (var z in indices.OrderByDescending(z => z))
        {
            newContent = newContent.RemoveAt(z);
        }
        return newContent;
    }

    static SeparatedSyntaxList<TNode> RemoveAtRange<TNode>(SeparatedSyntaxList<TNode> list, IEnumerable<int> indices) where TNode : SyntaxNode
    {
        var newContent = list;
        foreach (var z in indices.OrderByDescending(z => z))
        {
            newContent = newContent.RemoveAt(z);
        }
        return newContent;
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitArgumentList(ArgumentListSyntax node)
    {
        var @base = (ArgumentListSyntax)base.VisitArgumentList(node)!;
        var invalid = node.Arguments
            .Select((ps, i) => new { ps, i })
            .Where(v => removedParameters.Contains(v.ps.ToString()))
            .Select(t => t.i);

        var newList = RemoveAtRange(@base.Arguments, invalid);
        return @base.WithArguments(newList);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitForEachStatement(ForEachStatementSyntax node)
    {
        var @base = (ForEachStatementSyntax)base.VisitForEachStatement(node)!;
        return @base.WithAwaitKeyword(default).WithTriviaFrom(@base);
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
            var name = n.ToString();
            if (name.StartsWith(ReadOnlyMemory) || name.StartsWith(Memory))
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

        if (newType == type) // not replaced
        {
            newType = ProcessType(type);

            if (newType == type)
            {
                return @base;
            }
        }
        return @base.WithType(newType).WithTriviaFrom(@base);
    }
}
