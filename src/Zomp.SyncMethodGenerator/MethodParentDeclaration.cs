namespace Zomp.SyncMethodGenerator;

/// <summary>
/// Represents a class a <see cref="MethodToGenerate"/> belongs to.
/// </summary>
/// <param name="MethodParent">Type of container.</param>
/// <param name="ClassOrStructKeyword">Indicates whether struct or class are explicitly specified for a record.</param>
/// <param name="ParentName">Class name.</param>
/// <param name="Modifiers">A list of modifiers.</param>
/// <param name="TypeParameterListSyntax">A list of type parameters.</param>
internal sealed record MethodParentDeclaration(MethodParent MethodParent, SyntaxToken ClassOrStructKeyword, string ParentName, EquatableArray<ushort> Modifiers, EquatableArray<string> TypeParameterListSyntax);
