namespace Zomp.SyncMethodGenerator.Cloning;

/// <summary>
/// The signature a cloned method will be emitted with, and where the method it came from was
/// written. Two methods sharing a <see cref="Key"/> cannot both be emitted: the compiler would
/// see the same member declared twice.
/// </summary>
/// <param name="Key">Fully qualified signature of the method which will be emitted.</param>
/// <param name="FilePath">File path of the method being cloned.</param>
/// <param name="TextSpan">Text span of the method being cloned.</param>
/// <param name="LineSpan">Line span of the method being cloned.</param>
internal sealed record MethodSignature(string Key, string FilePath, TextSpan TextSpan, LinePositionSpan LineSpan)
{
    /// <summary>
    /// Compares by <see cref="Key"/> alone. The spans move whenever anything above the method is
    /// edited, and letting that reach the incremental pipeline would defeat caching for a method
    /// whose generated output has not changed at all.
    /// </summary>
    /// <param name="other">Signature to compare against.</param>
    /// <returns>True if both describe the same emitted member.</returns>
    public bool Equals(MethodSignature? other)
        => other is not null && string.Equals(Key, other.Key, StringComparison.Ordinal);

    /// <inheritdoc/>
    public override int GetHashCode() => Key.GetHashCode();

    /// <summary>
    /// Describes the method which is about to be emitted, precisely enough to tell whether two
    /// of them would declare the same member. The rewritten declaration is used rather than the
    /// original symbol, so the comparison sees exactly what the compiler will see - parameters
    /// already dropped, types already substituted.
    /// </summary>
    /// <param name="rewritten">Result of rewriting the method.</param>
    /// <param name="location">Where the original method sits.</param>
    /// <param name="original">Method being cloned, for the location to report against.</param>
    /// <returns>The signature, or null when the rewritten method could not be located.</returns>
    public static MethodSignature? Create(SyntaxNode rewritten, MethodLocation location, MethodDeclarationSyntax original)
    {
        var method = rewritten as MethodDeclarationSyntax
            ?? rewritten.DescendantNodes().OfType<MethodDeclarationSyntax>().FirstOrDefault();

        if (method is null)
        {
            return null;
        }

        // Arity by count rather than by name, since overloads which differ only in what they
        // call their type parameters still declare the same member.
        var arity = method.TypeParameterList?.Parameters.Count ?? 0;
        var arityMarker = arity > 0 ? "`" + arity.ToString(System.Globalization.CultureInfo.InvariantCulture) : string.Empty;

        var parameters = string.Join(",", method.ParameterList.Parameters.Select(static p => p.Type?.ToString() ?? string.Empty));

        var key = $"{location.Scope}.{method.Identifier.ValueText}{arityMarker}({parameters})";

        var sourceLocation = original.GetLocation();

        return new(key, sourceLocation.SourceTree?.FilePath ?? string.Empty, sourceLocation.SourceSpan, sourceLocation.GetLineSpan().Span);
    }

    /// <summary>
    /// Picks out the signatures produced by more than one method. Emitting all of them would
    /// declare the same member twice.
    /// </summary>
    /// <param name="keys">Signature of every method being generated.</param>
    /// <returns>The signatures which appear more than once.</returns>
    public static EquatableArray<string> FindCollisions(ImmutableArray<string?> keys)
    {
        var colliding = keys
            .Where(static key => key is not null)
            .GroupBy(static key => key!, StringComparer.Ordinal)
            .Where(static group => group.Count() > 1)
            .Select(static group => group.Key);

#if ROSLYN_4_12_OR_GREATER
        return new([.. colliding]);
#else
        return new(ImmutableArray.CreateRange(colliding));
#endif
    }

    /// <summary>
    /// Builds the diagnostic reported when this signature is produced more than once.
    /// </summary>
    /// <param name="descriptor">Descriptor of the diagnostic.</param>
    /// <returns>A new <see cref="ReportedDiagnostic"/>.</returns>
    public ReportedDiagnostic ToDiagnostic(DiagnosticDescriptor descriptor)
        => new(descriptor, FilePath, TextSpan, LineSpan, Key);
}
