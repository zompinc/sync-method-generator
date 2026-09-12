using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Zomp.SyncMethodGenerator.Cloning;

/// <summary>
/// Rewrites a method so that a copy of it compiles in a file of its own. Every type and static
/// member the method names is fully qualified, so that it resolves to the same symbol in the file
/// the copy is emitted to as in the file the method was written in. A transformation derives from
/// this class to change what the copy does.
/// </summary>
/// <param name="semanticModel">The semantic model.</param>
/// <param name="targetMethod">The method declaration to rewrite; other method declarations are dropped.</param>
internal abstract class CloningRewriter(SemanticModel semanticModel, MethodDeclarationSyntax targetMethod) : CSharpSyntaxRewriter
{
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

    /// <summary>
    /// Gets the semantic model of the tree the method is in.
    /// </summary>
    protected SemanticModel SemanticModel { get; } = semanticModel;

    /// <inheritdoc/>
    public override SyntaxNode? VisitArgument(ArgumentSyntax node)
    {
        var @base = (ArgumentSyntax)base.VisitArgument(node)!;

        // Handles nameof(Type)
        return GetSymbol(node.Expression) is ITypeSymbol typeSymbol && !TypeAlreadyQualified(typeSymbol)
            ? @base.WithExpression(ProcessSymbol(typeSymbol)).WithTriviaFrom(@base)
            : @base;
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitArrayType(ArrayTypeSyntax node)
    {
        var @base = (ArrayTypeSyntax)base.VisitArrayType(node)!;
        var elementType = TypeAlreadyQualified(node.ElementType)
            ? @base.ElementType
            : ProcessType(@base.ElementType);
        return @base.WithElementType(elementType).WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
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

    /// <inheritdoc/>
    public override SyntaxNode? VisitBinaryExpression(BinaryExpressionSyntax node)
    {
        var @base = (BinaryExpressionSyntax)base.VisitBinaryExpression(node)!;

        if (@base.OperatorToken.IsKind(SyntaxKind.IsKeyword) || @base.OperatorToken.IsKind(SyntaxKind.AsKeyword))
        {
            if (GetSymbol(node.Left) is IFieldSymbol leftSymbol)
            {
                @base = @base.WithLeft(ProcessSymbol(leftSymbol).WithTriviaFrom(node.Left));
            }

            if (GetSymbol(node.Right) is ISymbol symbol)
            {
                @base = @base.WithRight(ProcessSymbol(symbol).WithTriviaFrom(node.Right));
            }

            return @base.WithTriviaFrom(@base);
        }

        return @base;
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitCastExpression(CastExpressionSyntax node)
    {
        var @base = (CastExpressionSyntax)base.VisitCastExpression(node)!;
        return TypeAlreadyQualified(node.Type) ? @base : @base.WithType(ProcessType(node.Type)).WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitCatchDeclaration(CatchDeclarationSyntax node)
    {
        var @base = (CatchDeclarationSyntax)base.VisitCatchDeclaration(node)!;
        return @base.WithType(ProcessType(node.Type)).WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitConstantPattern(ConstantPatternSyntax node)
    {
        var @base = (ConstantPatternSyntax)base.VisitConstantPattern(node)!;
        return node.Expression switch
        {
            LiteralExpressionSyntax or MemberAccessExpressionSyntax => @base,
            _ => SemanticModel.GetTypeInfo(node.Expression).Type is { } type
                ? @base.WithExpression(ProcessSymbol(type).WithTriviaFrom(@base))
                : @base,
        };
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitDeclarationExpression(DeclarationExpressionSyntax node)
    {
        var @base = (DeclarationExpressionSyntax)base.VisitDeclarationExpression(node)!;
        return TypeAlreadyQualified(node.Type) ? @base : @base.WithType(ProcessType(node.Type)).WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitDeclarationPattern(DeclarationPatternSyntax node)
    {
        var @base = (DeclarationPatternSyntax)base.VisitDeclarationPattern(node)!;
        return TypeAlreadyQualified(node.Type) ? @base : @base.WithType(ProcessType(node.Type)).WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitForEachStatement(ForEachStatementSyntax node)
    {
        var @base = (ForEachStatementSyntax)base.VisitForEachStatement(node)!;
        return TypeAlreadyQualified(node.Type) ? @base : @base.WithType(ProcessType(node.Type)).WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitGenericName(GenericNameSyntax node)
    {
        var @base = (GenericNameSyntax)base.VisitGenericName(node)!;

        if (GetSymbol(node) is not INamedTypeSymbol symbol)
        {
            return @base;
        }

        var identifier = symbol switch
        {
            { ContainingSymbol: INamedTypeSymbol { IsGenericType: true } parentSymbol }
            => parentSymbol.ToDisplayString(GlobalDisplayFormatWithTypeParameters) + "." + symbol.Name,
            _ => symbol.ToDisplayString(GlobalDisplayFormat),
        };

        return @base.WithIdentifier(Identifier(identifier)).WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitIdentifierName(IdentifierNameSyntax node)
    {
        var @base = (IdentifierNameSyntax)base.VisitIdentifierName(node)!;

        if (node.Parent is not MemberAccessExpressionSyntax)
        {
            var symbol = GetSymbol(node);
            if (symbol is { IsStatic: true, ContainingType: { } containingType } memberSymbol)
            {
                if (symbol is IFieldSymbol or IMethodSymbol { MethodKind: not MethodKind.LocalFunction })
                {
                    var typeString = containingType.ToDisplayString(GlobalDisplayFormatWithTypeParameters);
                    return @base.WithIdentifier(Identifier($"{typeString}.{memberSymbol.Name}")).WithTriviaFrom(node);
                }
            }
        }

        if (node.Parent is TypeArgumentListSyntax)
        {
            return ProcessType(node);
        }

        return @base;
    }

    /// <summary>
    /// Parenthesizes every interpolated expression. A fully qualified name contains <c>::</c>,
    /// whose first colon would otherwise start a format string.
    /// </summary>
    /// <param name="node">The interpolation.</param>
    /// <returns>The interpolation with its expression parenthesized.</returns>
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
    public override SyntaxNode? VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
    {
        var @base = (MemberAccessExpressionSyntax)base.VisitMemberAccessExpression(node)!;

        // Rewrite static invocation (eg. File.ReadAllText)
        if (GetSymbol(node.Expression) is ITypeSymbol && node.Expression is TypeSyntax type)
        {
            var newType = ProcessType(type);
            if (newType != type)
            {
                @base = @base.WithExpression(newType);
            }
        }

        return @base;
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitMethodDeclaration(MethodDeclarationSyntax node)
        => node == targetMethod ? base.VisitMethodDeclaration(node) : null;

    /// <inheritdoc/>
    public override SyntaxNode? VisitNullableType(NullableTypeSyntax node)
    {
        var @base = (NullableTypeSyntax)base.VisitNullableType(node)!;

        return TypeAlreadyQualified(node.ElementType) ? @base : @base.WithElementType(ProcessType(@base.ElementType)).WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitObjectCreationExpression(ObjectCreationExpressionSyntax node)
    {
        var @base = (ObjectCreationExpressionSyntax)base.VisitObjectCreationExpression(node)!;

        if (SemanticModel.GetTypeInfo(node).Type is not { } t
            || t is INamedTypeSymbol { IsGenericType: true })
        {
            return @base;
        }

        var newType = ProcessSymbol(t);
        return newType == node.Type ? @base : @base.WithType(newType);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitParameter(ParameterSyntax node)
    {
        var @base = (ParameterSyntax)base.VisitParameter(node)!;

        return node.Type is null || TypeAlreadyQualified(node.Type) ? @base
            : @base.WithType(ProcessType(node.Type)).WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitQualifiedName(QualifiedNameSyntax node)
    {
        var @base = (QualifiedNameSyntax)base.VisitQualifiedName(node)!;

        return @base.Right is GenericNameSyntax ? @base.Right : (SyntaxNode)ProcessType(node);
    }

    /// <inheritdoc/>
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

    /// <inheritdoc/>
    public override SyntaxNode? VisitTypeConstraint(TypeConstraintSyntax node)
    {
        var @base = (TypeConstraintSyntax)base.VisitTypeConstraint(node)!;
        var newType = ProcessType(@base.Type);
        return newType == @base.Type ? @base : @base.WithType(newType).WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitTypeOfExpression(TypeOfExpressionSyntax node)
    {
        var @base = (TypeOfExpressionSyntax)base.VisitTypeOfExpression(node)!;
        return TypeAlreadyQualified(node.Type) ? @base : @base.WithType(ProcessType(node.Type)).WithTriviaFrom(@base);
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitVariableDeclaration(VariableDeclarationSyntax node)
    {
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

        return @base.WithType(newType).WithTriviaFrom(@base);
    }

    /// <summary>
    /// Prefixes a fully qualified name with the global alias.
    /// </summary>
    /// <param name="type">Fully qualified name.</param>
    /// <returns>The name, prefixed with <c>global::</c>.</returns>
    protected static string Global(string type) => $"global::{type}";

    /// <summary>
    /// Removes the preprocessor directives, and the text they disable, from the trivia which
    /// leads a method. The copy is emitted without whatever surrounded the original.
    /// </summary>
    /// <param name="trivia">Leading trivia of the method.</param>
    /// <returns>The trivia without preprocessor directives.</returns>
    protected static SyntaxTriviaList RemovePreprocessorDirectives(SyntaxTriviaList trivia)
    {
        static bool Preprocessors(SyntaxTrivia st)
            => st.IsKind(SyntaxKind.IfDirectiveTrivia)
            || st.IsKind(SyntaxKind.ElifDirectiveTrivia)
            || st.IsKind(SyntaxKind.ElseDirectiveTrivia)
            || st.IsKind(SyntaxKind.EndIfDirectiveTrivia)
            || st.IsKind(SyntaxKind.RegionDirectiveTrivia)
            || st.IsKind(SyntaxKind.EndRegionDirectiveTrivia)
            || st.IsKind(SyntaxKind.DisabledTextTrivia);

        while (trivia.FirstOrDefault(Preprocessors) is { } preprocessor
            && preprocessor != default)
        {
            trivia = trivia.Remove(preprocessor);
        }

        return trivia;
    }

    /// <summary>
    /// Checks whether a type is written in a form which its own visitor qualifies.
    /// </summary>
    /// <param name="type">The type.</param>
    /// <returns>True if the type needs nothing more.</returns>
    protected static bool TypeAlreadyQualified(TypeSyntax type)
        => type is NullableTypeSyntax or GenericNameSyntax or TupleTypeSyntax or ArrayTypeSyntax or QualifiedNameSyntax;

    /// <summary>
    /// Gets the symbol a node refers to.
    /// </summary>
    /// <param name="node">The node.</param>
    /// <returns>The symbol, or null if the node does not bind to one.</returns>
    protected ISymbol? GetSymbol(SyntaxNode node) => SemanticModel.GetSymbolInfo(node).Symbol;

    /// <summary>
    /// Gets the fully qualified name of a type.
    /// </summary>
    /// <param name="symbol">The type, or any other symbol, which is named as it is.</param>
    /// <returns>The name.</returns>
    protected string MakeType(ISymbol symbol)
        => symbol switch
        {
            INamedTypeSymbol nts => MapTypeName(nts) ?? symbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat),
            _ => symbol.Name,
        };

    /// <summary>
    /// Names a symbol so that it resolves wherever the copy is emitted.
    /// </summary>
    /// <param name="typeSymbol">The symbol.</param>
    /// <returns>The name.</returns>
    protected SimpleNameSyntax ProcessSymbol(ISymbol typeSymbol) => MapSymbol(typeSymbol) ?? typeSymbol switch
    {
        INamedTypeSymbol nts => IdentifierName(MakeType(nts)),
        IArrayTypeSymbol ats => IdentifierName(MakeType(ats.ElementType) + $"[{new string(',', ats.Rank - 1)}]"),
        IFieldSymbol fs => IdentifierName(MakeType(fs.Type) + '.' + fs.Name),
        _ => IdentifierName(typeSymbol.Name),
    };

    /// <summary>
    /// Qualifies a type written as a simple or qualified name.
    /// </summary>
    /// <param name="typeSyntax">The type.</param>
    /// <returns>The qualified type, or the type itself if it needs nothing.</returns>
    protected TypeSyntax ProcessType(TypeSyntax typeSyntax) => typeSyntax switch
    {
        IdentifierNameSyntax { Identifier.ValueText: "var" } => typeSyntax,
        IdentifierNameSyntax or QualifiedNameSyntax => ProcessSyntaxUsingSymbol(typeSyntax),
        _ => typeSyntax,
    };

    /// <summary>
    /// Gives a transformation the chance to name a symbol differently in the copy.
    /// </summary>
    /// <param name="symbol">The symbol.</param>
    /// <returns>The name to use, or null to use the fully qualified name.</returns>
    protected virtual SimpleNameSyntax? MapSymbol(ISymbol symbol) => null;

    /// <summary>
    /// Gives a transformation the chance to substitute one type for another wherever a type is
    /// named by its fully qualified name.
    /// </summary>
    /// <param name="symbol">The type.</param>
    /// <returns>The name to use, or null to use the fully qualified name.</returns>
    protected virtual string? MapTypeName(INamedTypeSymbol symbol) => null;

    private static bool TypeAlreadyQualified(ITypeSymbol type)
        => type is INamedTypeSymbol namedType
            && namedType is { IsGenericType: true };

    private TypeSyntax ProcessSyntaxUsingSymbol(TypeSyntax typeSyntax)
    {
        var typeSymbol = SemanticModel.GetTypeInfo(typeSyntax).Type;
        return typeSymbol is null ? typeSyntax : ProcessSymbol(typeSymbol).WithTriviaFrom(typeSyntax);
    }
}
