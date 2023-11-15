namespace Zomp.SyncMethodGenerator;

/// <summary>
/// All types that an IAsyncEnumerable can be converted into.
/// </summary>
[Flags]
public enum CollectionTypes
{
    /// <summary>
    /// Type for System.Collections.Generic.IEnumerable .
    /// </summary>
    IEnumerable = 1,

    /// <summary>
    /// Type for System.Collections.Generic.IList .
    /// </summary>
    IList = 2,

    /// <summary>
    /// Type for System.ReadOnlySpan .
    /// </summary>
    ReadOnlySpan = 4,

    /// <summary>
    /// Type for System.Span .
    /// </summary>
    Span = 8,
}
