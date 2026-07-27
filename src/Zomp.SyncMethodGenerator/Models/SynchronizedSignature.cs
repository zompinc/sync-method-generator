namespace Zomp.SyncMethodGenerator.Models;

/// <summary>
/// The signature a synchronized method will be emitted with, and where the method it came from
/// was written. Two methods sharing a <see cref="Key"/> cannot both be emitted: the compiler
/// would see the same member declared twice.
/// </summary>
/// <param name="Key">Fully qualified signature of the method which will be emitted.</param>
/// <param name="FilePath">File path of the method being synchronized.</param>
/// <param name="TextSpan">Text span of the method being synchronized.</param>
/// <param name="LineSpan">Line span of the method being synchronized.</param>
internal sealed record SynchronizedSignature(string Key, string FilePath, TextSpan TextSpan, LinePositionSpan LineSpan)
{
    /// <summary>
    /// Compares by <see cref="Key"/> alone. The spans move whenever anything above the method is
    /// edited, and letting that reach the incremental pipeline would defeat caching for a method
    /// whose generated output has not changed at all.
    /// </summary>
    /// <param name="other">Signature to compare against.</param>
    /// <returns>True if both describe the same emitted member.</returns>
    public bool Equals(SynchronizedSignature? other)
        => other is not null && string.Equals(Key, other.Key, StringComparison.Ordinal);

    /// <inheritdoc/>
    public override int GetHashCode() => Key.GetHashCode();

    /// <summary>
    /// Builds the diagnostic reported when this signature is produced more than once.
    /// </summary>
    /// <returns>A new <see cref="ReportedDiagnostic"/>.</returns>
    public ReportedDiagnostic ToCollisionDiagnostic()
        => new(DiagnosticMessages.CollidingOverloads, FilePath, TextSpan, LineSpan, Key);
}
