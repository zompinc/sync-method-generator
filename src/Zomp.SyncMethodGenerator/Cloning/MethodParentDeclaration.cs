namespace Zomp.SyncMethodGenerator.Cloning;

/// <summary>
/// Represents a type a <see cref="MethodLocation"/> is nested in.
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

    /// <summary>
    /// Gets the name of the type with its type parameters, so that <c>Class</c>, <c>Class{T}</c>
    /// and <c>Class{T,T2}</c> stay distinct.
    /// </summary>
    public string ScopeName => TypeParameterListSyntax.IsEmpty
        ? ParentName
        : ParentName + "{" + string.Join(",", TypeParameterListSyntax) + "}";
}
