namespace Zomp.SyncMethodGenerator.Cloning;

/// <summary>
/// A method marked for cloning, either by an attribute of its own or by one on its containing type.
/// </summary>
/// <param name="Context">Context of the attribute which marked the method.</param>
/// <param name="Syntax">Declaration of the method.</param>
internal sealed record CloneTarget(GeneratorAttributeSyntaxContext Context, MethodDeclarationSyntax Syntax)
{
    /// <summary>
    /// Finds the methods an attribute marks. One on a type marks every method the type declares.
    /// </summary>
    /// <param name="syntaxProvider">Syntax provider of the generator.</param>
    /// <param name="fullyQualifiedMetadataName">Name of the attribute.</param>
    /// <returns>A method for each method marked.</returns>
    public static IncrementalValuesProvider<CloneTarget> ForAttribute(SyntaxValueProvider syntaxProvider, string fullyQualifiedMetadataName)
        => syntaxProvider
            .ForAttributeWithMetadataName(
                fullyQualifiedMetadataName,
                predicate: static (s, _) => IsCandidate(s),
                transform: static (ctx, ct) => Expand(ctx, ct))
            .SelectMany((list, ct) => list);

    private static bool IsCandidate(SyntaxNode node) => node switch
    {
        MethodDeclarationSyntax { AttributeLists.Count: > 0 } => true,
        ClassDeclarationSyntax { AttributeLists.Count: > 0 } => true,
        StructDeclarationSyntax { AttributeLists.Count: > 0 } => true,
        InterfaceDeclarationSyntax { AttributeLists.Count: > 0 } => true,
        RecordDeclarationSyntax { AttributeLists.Count: > 0 } => true,
        _ => false,
    };

    private static ImmutableArray<CloneTarget> Expand(GeneratorAttributeSyntaxContext ctx, CancellationToken ct)
    {
        ct.ThrowIfCancellationRequested();

        if (ctx.TargetNode is TypeDeclarationSyntax typeDecl)
        {
#if ROSLYN_4_12_OR_GREATER
            return [.. typeDecl.Members.OfType<MethodDeclarationSyntax>().Select(s => new CloneTarget(ctx, s))];
#else
            return ImmutableArray.CreateRange(typeDecl.Members.OfType<MethodDeclarationSyntax>().Select(s => new CloneTarget(ctx, s)));
#endif
        }
        else if (ctx.TargetNode is MethodDeclarationSyntax methodDecl)
        {
#if ROSLYN_4_12_OR_GREATER
            return [new CloneTarget(ctx, methodDecl)];
#else
            return ImmutableArray.Create(new CloneTarget(ctx, methodDecl));
#endif
        }

#if ROSLYN_4_12_OR_GREATER
        return [];
#else
        return ImmutableArray<CloneTarget>.Empty;
#endif
    }
}
