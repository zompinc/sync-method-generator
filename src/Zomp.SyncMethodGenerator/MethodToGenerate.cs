namespace Zomp.SyncMethodGenerator;

/// <summary>
/// Represents a sync method to generate from its async version.
/// </summary>
/// <param name="Namespaces">List of namespaces this method is under.</param>
/// <param name="IsNamespaceFileScoped">True if namespace is file scoped.</param>
/// <param name="Classes">List of classes this method belongs to starting from the outer-most class.</param>
/// <param name="MethodName">Name of the method.</param>
/// <param name="Implementation">Implementation.</param>
/// <param name="DisableNullable">Disables nullable for the method.</param>
internal sealed record MethodToGenerate(
    IEnumerable<string> Namespaces,
    bool IsNamespaceFileScoped,
    IEnumerable<ClassDeclaration> Classes,
    string MethodName,
    string Implementation,
    bool DisableNullable);
