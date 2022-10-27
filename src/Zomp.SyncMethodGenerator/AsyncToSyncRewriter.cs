namespace Zomp.SyncMethodGenerator;

/// <summary>
/// Rewrites a method synchronously
/// </summary>
public class AsyncToSyncRewriter : CSharpSyntaxRewriter
{
    private readonly SemanticModel semanticModel;
    readonly HashSet<string> removedParameters = new();
    readonly HashSet<string> memoryToSpan = new();

    const string ReadOnlyMemory = "System.ReadOnlyMemory";
    const string Memory = "System.Memory";
    const string TaskType = "System.Threading.Tasks.Task";
    const string ValueTaskType = "System.Threading.Tasks.ValueTask";

    private readonly Dictionary<string, string?> Replacements = new()
    {
        { "System.Collections.Generic.IAsyncEnumerable", "System.Collections.Generic.IEnumerable" },
        { "System.Collections.Generic.IAsyncEnumerator", "System.Collections.Generic.IEnumerator" },
        { TaskType, null},
        { ValueTaskType, null},
        { ReadOnlyMemory, "System.ReadOnlySpan"},
        { Memory, "System.Span"},
    };

    /// <summary>
    /// Creates a new instance of <see cref="AsyncToSyncRewriter"/>.
    /// </summary>
    /// <param name="semanticModel"></param>
    public AsyncToSyncRewriter(SemanticModel semanticModel)
    {
        this.semanticModel = semanticModel;
    }

    static string MakeType(ISymbol symbol)
        => "global::" + symbol switch
        {
            INamedTypeSymbol nts when nts.IsGenericType is true => Regex.Replace(symbol.ToDisplayString(), "<.*", ""),
            _ => symbol.ToDisplayString()
        };

    TypeSyntax MakeGlobalIfNecessary(TypeSyntax type)
    {
        if (type is PredefinedTypeSyntax
            || type is TupleTypeSyntax
            || type is IdentifierNameSyntax ins && ins.Identifier.ValueText == "var")
        {
            return type;
        }

        if (type is NullableTypeSyntax nuts)
        {
            var typeInside = MakeGlobalIfNecessary(nuts.ElementType);
            if (typeInside == nuts.ElementType)
                return type;
            return nuts.WithElementType(typeInside);
        }

        if (semanticModel
            .GetSymbolInfo(type)
            .Symbol is INamedTypeSymbol nts)
        {
            return SyntaxFactory.IdentifierName(MakeType(nts))
                .WithLeadingTrivia(type.GetLeadingTrivia())
                .WithTrailingTrivia(type.GetTrailingTrivia());
        }

        return type;
    }

    static TypeSyntax ProcessSymbol(ITypeSymbol typeSymbol) => typeSymbol switch
    {
        INamedTypeSymbol nts => SyntaxFactory.IdentifierName(MakeType(nts)),
        IArrayTypeSymbol ats => SyntaxFactory.IdentifierName(MakeType(ats.ElementType) + "[]"),
        _ => SyntaxFactory.IdentifierName(typeSymbol.Name),
    };

    private TypeSyntax ProcessType(TypeSyntax typeSyntax) => typeSyntax switch
    {
        PredefinedTypeSyntax or TupleTypeSyntax or GenericNameSyntax or ArrayTypeSyntax => typeSyntax,
        _ => semanticModel.GetTypeInfo(typeSyntax).Type is { } symbol ? ProcessSymbol(symbol) : typeSyntax
    };

    static string GetNameWithoutTypeParams(ISymbol symbol)
        => symbol.ContainingNamespace + "." + symbol.Name;

    /// <inheritdoc/>
    public override SyntaxNode? VisitGenericName(GenericNameSyntax node)
    {
        var @base = (GenericNameSyntax)base.VisitGenericName(node)!;

        var symbol = semanticModel.GetSymbolInfo(node).Symbol;

        string? convert(string key)
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
                .WithLeadingTrivia(@base.GetLeadingTrivia())
                .WithTrailingTrivia(@base.GetTrailingTrivia());
        }
        else
        {
            return @base.TypeArgumentList.Arguments[0]
                .WithLeadingTrivia(@base.GetLeadingTrivia())
                .WithTrailingTrivia(@base.GetTrailingTrivia());
        }
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
    public override SyntaxNode? VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
    {
        var @base = (MemberAccessExpressionSyntax)base.VisitMemberAccessExpression(node)!;

        if (@base.Name.Identifier.ValueText == "Span"
            && @base.Expression is IdentifierNameSyntax ins && memoryToSpan.Contains(ins.Identifier.ValueText))
        {
            return @base.Expression;
        }
        else if (@base.Name.Identifier.ValueText.EndsWith("Async"))
        {
            return @base.WithName(SyntaxFactory.IdentifierName(RemoveAsync(@base.Name.Identifier.ValueText)));
        }
        return @base;
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitInvocationExpression(InvocationExpressionSyntax node)
    {
        var @base = (InvocationExpressionSyntax)base.VisitInvocationExpression(node)!;

        if (@base.Expression is not MemberAccessExpressionSyntax maes
            || maes.Name is not IdentifierNameSyntax ins)
        {
            return @base;
        }

        var originalSymbol = node.Expression is MemberAccessExpressionSyntax zz ? semanticModel.GetSymbolInfo(zz).Symbol as IMethodSymbol : null;

        if (maes.Name is IdentifierNameSyntax { Identifier.ValueText: "WithCancellation" or "ConfigureAwait" })
        {
            return maes.Expression;
        }

        string? newName = null;
        if (originalSymbol is { Name: "AsMemory", ContainingNamespace.Name: "System" })
        {
            newName = "AsSpan";
        }
        else if (ins.Identifier.ValueText.EndsWith("Async") || ins.Identifier.ValueText == "GetAsyncEnumerator")
        {
            newName = RemoveAsync(ins.Identifier.ValueText);
        }
        else if (ins.Identifier.ValueText == "FromResult" && @base.ArgumentList.Arguments.Count > 0)
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
        return @base.Expression;
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitArrayType(ArrayTypeSyntax node)
    {
        var @base = (ArrayTypeSyntax)base.VisitArrayType(node)!;
        var elementType = @base.ElementType;
        if (elementType is PredefinedTypeSyntax)
        {
            return @base;
        }
        var symbol = semanticModel.GetTypeInfo(elementType).Type;
        if (symbol is null)
        {
            return @base;
        }
        var newType = SyntaxFactory.IdentifierName(MakeType(symbol));
        return @base.WithElementType(newType);
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
            @base.Arguments.Select(z
                    => ProcessType(z)),
            @base.Arguments.GetSeparators()
            ));
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitTypeConstraint(TypeConstraintSyntax node)
    {
        var @base = (TypeConstraintSyntax)base.VisitTypeConstraint(node)!;
        var newType = ProcessType(@base.Type);
        if (newType == @base.Type)
            return @base;

        return @base.WithType(newType)
            .WithLeadingTrivia(@base.GetLeadingTrivia())
            .WithTrailingTrivia(@base.GetTrailingTrivia());
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitMethodDeclaration(MethodDeclarationSyntax node)
    {
        var attrinutes = node.AttributeLists.Select(
            z => SyntaxFactory.AttributeList(SyntaxFactory.SeparatedList(z.Attributes.Where(ShouldPreserveAttribute)))
            .WithTrailingTrivia(z.GetLeadingTrivia())
            .WithTrailingTrivia(z.GetTrailingTrivia())
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

        if (!isTask && !hasAsync && genericReturnType is not null)
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
            var fullName = attributeContainingTypeSymbol.ToDisplayString();

            // Is the attribute [CreateSyncVersion] attribute?
            return fullName != SyncMethodSourceGenerator.CreateSyncVersionAttribute;
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

        var newReturnType = isTask
            ? SyntaxFactory.IdentifierName("void")
                .WithLeadingTrivia(returnType.GetLeadingTrivia())
                .WithTrailingTrivia(returnType.GetTrailingTrivia())
            : @base.ReturnType;

        var retval = @base
            .WithIdentifier(SyntaxFactory.Identifier(newName))
            .WithReturnType(newReturnType)
            .WithModifiers(SyntaxFactory.TokenList(modifiers))
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
        var @foreach = (ForEachStatementSyntax)base.VisitForEachStatement(node)!;

        return @foreach.WithAwaitKeyword(default)
            .WithLeadingTrivia(@foreach.GetLeadingTrivia())
            .WithTrailingTrivia(@foreach.GetTrailingTrivia());
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
    {
        var @base = (LocalDeclarationStatementSyntax)base.VisitLocalDeclarationStatement(node)!;
        return @base.WithAwaitKeyword(default)
            .WithLeadingTrivia(@base.GetLeadingTrivia())
            .WithTrailingTrivia(@base.GetTrailingTrivia());
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitVariableDeclaration(VariableDeclarationSyntax node)
    {
        var @base = (VariableDeclarationSyntax)base.VisitVariableDeclaration(node)!;
        var type = node.Type;
        var newType = @base.Type;

        if (newType == type) // not replaced
        {
            newType = MakeGlobalIfNecessary(type);

            if (newType == type)
            {
                return @base;
            }
        }
        return @base.WithType(newType)
            .WithLeadingTrivia(@base.GetLeadingTrivia())
            .WithTrailingTrivia(@base.GetTrailingTrivia());
    }
}
