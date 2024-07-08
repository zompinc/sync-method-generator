namespace Zomp.SyncMethodGenerator;

/// <summary>
/// Represents a class a <see cref="MethodToGenerate"/> belongs to.
/// </summary>
/// <param name="MethodParent">Type of container.</param>
/// <param name="ParentName">Class name.</param>
/// <param name="Modifiers">A list of modifiers.</param>
/// <param name="TypeParameterListSyntax">A list of type parameters.</param>
/// <param name="ClassOrStructKeyword">Indicates whether struct or class are explicitly specified for a record.</param>
internal sealed record MethodParentDeclaration(MethodParent MethodParent, string ParentName, EquatableArray<ushort> Modifiers, EquatableArray<string> TypeParameterListSyntax, SyntaxToken ClassOrStructKeyword)
{
    public MethodParentDeclaration(MethodParent methodParent, SyntaxToken parentName, SyntaxTokenList modifiers, TypeParameterListSyntax? typeParameterList, SyntaxToken classOrStructKeyword = default)
        : this(
            methodParent,
            parentName.ValueText,
            modifiers.Select(z => (ushort)z.RawKind).Where(z => z != (ushort)SyntaxKind.PartialKeyword).ToImmutableArray(),
            (typeParameterList is null ? [] : typeParameterList.Parameters.Select(z => z.Identifier.ValueText)).ToImmutableArray(),
            classOrStructKeyword)
    {
    }
}
