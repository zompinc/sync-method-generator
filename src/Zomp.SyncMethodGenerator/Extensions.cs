namespace Zomp.SyncMethodGenerator;

internal static class Extensions
{
    /// <summary>
    /// Gets array of indices that match a predicate.
    /// </summary>
    /// <typeparam name="T">The type.</typeparam>
    /// <param name="elements">Original elements.</param>
    /// <param name="predicate">The predicate.</param>
    /// <returns>Array of indices that match a predicate.</returns>
    internal static int[] GetIndices<T>(this IEnumerable<T> elements, Func<T, int, bool> predicate)
    => elements.Select((ps, i) => (ps, i))
        .Where((elem, i) => predicate(elem.ps, i))
        .Select(t => t.i)
        .ToArray();

    internal static bool EndsWithAsync(this string str)
        => str.EndsWith("Async", StringComparison.Ordinal);
}
