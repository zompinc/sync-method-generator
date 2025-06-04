namespace Zomp.SyncMethodGenerator;

/// <summary>
/// Represents a sync method to generate from its async version.
/// </summary>
/// <param name="Index">Index of the method in the source file.</param>
/// <param name="Namespaces">List of namespaces this method is under.</param>
/// <param name="IsNamespaceFileScoped">True if namespace is file scoped.</param>
/// <param name="IsCSharp14Extension">True if this is C# 14 extension.</param>
/// <param name="Parents">List of classes/structs/records this method belongs to starting from the outer-most class.</param>
/// <param name="MethodName">Name of the method.</param>
/// <param name="Implementation">Implementation.</param>
/// <param name="DisableNullable">Disables nullable for the method.</param>
/// <param name="Diagnostics">Diagnostics.</param>
/// <param name="HasErrors">True if there are errors in <see cref="Diagnostics"/>.</param>
internal sealed record MethodToGenerate(
    int Index,
    EquatableArray<string> Namespaces,
    bool IsNamespaceFileScoped,
    bool IsCSharp14Extension,
    EquatableArray<MethodParentDeclaration> Parents,
    string MethodName,
    string Implementation,
    bool DisableNullable,
    EquatableArray<ReportedDiagnostic> Diagnostics,
    bool HasErrors);
