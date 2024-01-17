namespace Zomp.SyncMethodGenerator;

/// <summary>
/// Represents a class a <see cref="MethodToGenerate"/> belongs to.
/// </summary>
/// <param name="ClassName">Class name.</param>
/// <param name="Modifiers">A list of modifiers.</param>
/// <param name="TypeParameterListSyntax">A list of type parameters.</param>
internal sealed record ClassDeclaration(string ClassName, EquatableArray<ushort> Modifiers, EquatableArray<string> TypeParameterListSyntax);
