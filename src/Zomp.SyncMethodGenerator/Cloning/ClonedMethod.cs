namespace Zomp.SyncMethodGenerator.Cloning;

/// <summary>
/// A method rewritten by a transformation, ready to be emitted into a file of its own.
/// </summary>
/// <param name="Location">Where the original method sits.</param>
/// <param name="MethodName">Name of the original method.</param>
/// <param name="Implementation">The rewritten method.</param>
/// <param name="DisableNullable">True to leave the <c>#nullable enable</c> directive out of the file.</param>
/// <param name="Diagnostics">Diagnostics the transformation reported.</param>
/// <param name="HasErrors">True if there are errors in <see cref="Diagnostics"/>, in which case nothing is emitted.</param>
/// <param name="Signature">Signature the rewritten method will be emitted with, used to detect colliding overloads.</param>
internal sealed record ClonedMethod(
    MethodLocation Location,
    string MethodName,
    string Implementation,
    bool DisableNullable,
    EquatableArray<ReportedDiagnostic> Diagnostics,
    bool HasErrors,
    MethodSignature? Signature)
{
    /// <summary>
    /// Creates a cloned method from the result of rewriting it.
    /// </summary>
    /// <param name="location">Where the original method sits.</param>
    /// <param name="original">Declaration of the original method.</param>
    /// <param name="rewritten">Result of rewriting the method, or the extension block which holds it.</param>
    /// <param name="disableNullable">True to leave the <c>#nullable enable</c> directive out of the file.</param>
    /// <param name="diagnostics">Diagnostics the transformation reported.</param>
    /// <returns>A new <see cref="ClonedMethod"/>.</returns>
    public static ClonedMethod Create(
        MethodLocation location,
        MethodDeclarationSyntax original,
        SyntaxNode rewritten,
        bool disableNullable,
        ImmutableArray<ReportedDiagnostic> diagnostics)
    {
        var hasErrors = false;
        foreach (var diagnostic in diagnostics)
        {
            hasErrors |= diagnostic.Descriptor.DefaultSeverity == DiagnosticSeverity.Error;
        }

        return new(
            location,
            original.Identifier.ValueText,
            rewritten.ToFullString(),
            disableNullable,
            diagnostics,
            hasErrors,
            MethodSignature.Create(rewritten, location, original));
    }
}
