namespace Zomp.SyncMethodGenerator;

/// <summary>
/// Rewrites a method synchronously
/// </summary>
public class AsyncToSyncRewriter : CSharpSyntaxRewriter
{
    private readonly SemanticModel semanticModel;
    readonly HashSet<string> removedParameters = new();
    readonly HashSet<string> memoryToSpan = new();

    static readonly string TaskType = typeof(Task).ToString();
    static readonly string MemoryType = "System.Memory";
    static readonly string ValueTaskType = typeof(ValueTask).ToString();
    static readonly string IAsyncEnumerator = "System.Collections.Generic.IAsyncEnumerator";

    /// <summary>
    /// Creates a new instance of <see cref="AsyncToSyncRewriter"/>.
    /// </summary>
    /// <param name="semanticModel"></param>
    public AsyncToSyncRewriter(SemanticModel semanticModel)
    {
        this.semanticModel = semanticModel;
    }

    static string MakeType(string @namespace, string type)
    {
        return $"global::{@namespace}.{type}";
    }

    static string MakeType(INamedTypeSymbol symbol)
    {
        return $"global::{symbol.ToDisplayString()}";
    }

    TypeSyntax ReplaceWithGlobal(TypeSyntax ts)
    {
        if (semanticModel.GetTypeInfo(ts).Type is INamedTypeSymbol symbol)
        {
            var globalType = MakeType(symbol);
            return SyntaxFactory.IdentifierName(globalType)
                .WithLeadingTrivia(ts.GetLeadingTrivia())
                .WithTrailingTrivia(ts.GetTrailingTrivia());
        }
        return ts;
    }

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

    TypeSyntax ReplaceType(TypeSyntax ts, string? variableName = null)
    {
        if (semanticModel.GetTypeInfo(ts).Type is not INamedTypeSymbol symbol)
            return ts;

        if (ts is GenericNameSyntax genericType)
        {
            var typeParams = genericType.TypeArgumentList.Arguments;

            var newTypeParams = new List<TypeSyntax>();

            foreach (var param in typeParams)
            {
                if (param is PredefinedTypeSyntax or TupleTypeSyntax)
                {
                    newTypeParams.Add(param);
                }
                else
                {
                    var typeParam = semanticModel.GetTypeInfo(param).Type as INamedTypeSymbol;
                    var zz = typeParam is not null
                        ? SyntaxFactory.IdentifierName(MakeType(typeParam))
                        : param;
                    newTypeParams.Add(zz);
                }
            }

            if (symbol.Name is "IAsyncEnumerable" or "IAsyncEnumerator"
            && symbol.ContainingNamespace.ToDisplayString().Equals("System.Collections.Generic", StringComparison.Ordinal))
            {
                return SyntaxFactory
                .GenericName(MakeType("System.Collections.Generic", RemoveAsync(symbol.Name)))
                .AddTypeArgumentListArguments(newTypeParams[0])
                .WithLeadingTrivia(ts.GetLeadingTrivia())
                .WithTrailingTrivia(ts.GetTrailingTrivia());
            }
            else if (symbol.Name is "Task" or "ValueTask")
            {
                if (newTypeParams is { Count: > 0 })
                {
                    var newType = newTypeParams[0];
                    if (newType is PredefinedTypeSyntax pts)
                    {
                        return pts
                            .WithLeadingTrivia(ts.GetLeadingTrivia())
                            .WithTrailingTrivia(ts.GetTrailingTrivia());
                    }
                }
            }
            else if (symbol.Name is "Memory" or "ReadOnlyMemory")
            {
                if (variableName is not null)
                {
                    memoryToSpan.Add(variableName);
                }
                return SyntaxFactory
                .GenericName(MakeType("System", symbol.Name.Replace("Memory", "Span")))
                .AddTypeArgumentListArguments(newTypeParams[0])
                .WithLeadingTrivia(ts.GetLeadingTrivia())
                .WithTrailingTrivia(ts.GetTrailingTrivia());
            }
        }
        return ts;
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitParameter(ParameterSyntax node)
    {
        var ps = (ParameterSyntax)base.VisitParameter(node)!;

        if (node.Type is not { } ts)
        {
            return ps;
        }
        return ps.WithType(ReplaceType(ts, ps.Identifier.ValueText));
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

        if (IsAsyncMethod(maes))
        {
            return maes.Expression;
        }

        var avava = semanticModel.GetTypeInfo(node);
        var resultingType = avava.Type!;

        if (resultingType is INamedTypeSymbol nts && nts.IsGenericType)
        {
            resultingType = nts.OriginalDefinition;
        }

        var typeStr = resultingType.ToString();

        typeStr = Regex.Replace(typeStr, "<[^>]*>", "");


        if (typeStr != TaskType && typeStr != ValueTaskType && typeStr != IAsyncEnumerator && typeStr != MemoryType)
        {
            return @base;
        }

        string? newName = null;
        if (originalSymbol is not null
            && originalSymbol.ContainingNamespace.Name == "System"
            && originalSymbol.Name == "AsMemory")
        {
            newName = "AsSpan";
        }
        else if (ins.Identifier.ValueText.EndsWith("Async") || ins.Identifier.ValueText == "GetAsyncEnumerator")
        {
            newName = RemoveAsync(ins.Identifier.ValueText);
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
    public override SyntaxNode? VisitWhileStatement(WhileStatementSyntax node)
    {
        var @base = (WhileStatementSyntax)base.VisitWhileStatement(node)!;
        return @base;
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
    public override SyntaxNode? VisitTypeConstraint(TypeConstraintSyntax node)
    {
        var @base = (TypeConstraintSyntax)base.VisitTypeConstraint(node)!;
        var type = @base.Type;
        var newType = ReplaceWithGlobal(type);
        var retval = @base.WithType(newType);
        return retval;
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitMethodDeclaration(MethodDeclarationSyntax node)
    {
        var attrinutes = node.AttributeLists.Select(
            z => SyntaxFactory.AttributeList(SyntaxFactory.SeparatedList(z.Attributes.Where(ShouldPreserveAttribute)))
            .WithTrailingTrivia(z.GetLeadingTrivia())
            .WithTrailingTrivia(z.GetTrailingTrivia())
            );

        var zz = SyntaxFactory.List(attrinutes.Where(z => z.Attributes.Any()));

        var newNode = base.VisitMethodDeclaration(node) as MethodDeclarationSyntax ?? throw new InvalidOperationException("Can't cast");
        var returnType = node.ReturnType;

        if (semanticModel.GetTypeInfo(returnType).Type is not INamedTypeSymbol symbol)
        {
            return newNode;
        }

        var isTask = symbol.ToString() == TaskType;

        var hasAsync = newNode.Modifiers.Any(z => z.IsKind(SyntaxKind.AsyncKeyword));

        if (!isTask && !hasAsync
            && returnType is not GenericNameSyntax genericReturnType)
        {
            return newNode;
        }

        var modifiers = newNode.Modifiers.Where(z => !z.IsKind(SyntaxKind.AsyncKeyword));

        var originalName = newNode.Identifier.Text;
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
        SyntaxTriviaList newTriviaList = trivia;
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

        var newType = isTask
            ? SyntaxFactory.IdentifierName("void")
                .WithLeadingTrivia(returnType.GetLeadingTrivia())
                .WithTrailingTrivia(returnType.GetTrailingTrivia())
            : ReplaceType(node.ReturnType);

        var retval = newNode
            .WithIdentifier(SyntaxFactory.Identifier(newName))
            .WithReturnType(newType)
            .WithModifiers(SyntaxFactory.TokenList(modifiers))
            .WithAttributeLists(zz)
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

    static bool IsAsyncMethod(MemberAccessExpressionSyntax maes)
        => maes.Name is IdentifierNameSyntax { Identifier.ValueText: "WithCancellation" or "ConfigureAwait" };

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
        var newType = ReplaceType(type);

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
