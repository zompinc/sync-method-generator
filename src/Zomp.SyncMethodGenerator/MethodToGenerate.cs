namespace Zomp.SyncMethodGenerator;

/// <summary>
/// Represetns a sync method to generate from its async version
/// </summary>
/// <param name="Namespaces">List of namespaces this method is under</param>
/// <param name="IsNamespaceFileScoped">True if namespace is file scoped</param>
/// <param name="Classes">List of classes this method belongs to startng from the outer-most class.</param>
/// <param name="MethodName">Name of the method</param>
/// <param name="Implementation">Implementation</param>
public record MethodToGenerate(
    IEnumerable<string> Namespaces,
    bool IsNamespaceFileScoped,
    IEnumerable<Class> Classes,
    string MethodName,
    string Implementation);

/// <summary>
/// Represents a class a <see cref="MethodToGenerate"/> belongs to
/// </summary>
/// <param name="ClassName">Class name</param>
/// <param name="Modifiers">A list of modifiers</param>
public record Class(string ClassName, IEnumerable<SyntaxKind> Modifiers);
