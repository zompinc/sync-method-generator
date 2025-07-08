//HintName: Test.Class.GetPoints.g.cs
public static global::System.Collections.Generic.IEnumerable<global::System.Drawing.Point> GetPoints<T>(global::System.Collections.Generic.IEnumerable<T[]> input, int start, int end, global::System.Func<T, bool> convertToBool)
{
    static global::System.Collections.Generic.IEnumerable<global::System.ReadOnlyMemory<bool>> Convert(global::System.Collections.Generic.IEnumerable<T[]> input, int start, int end, global::System.Func<T, bool> convertToBool)
    {
        foreach (var element in input)
        {
            yield return new global::System.ReadOnlyMemory<bool>([.. global::System.Linq.Enumerable.Select(global::System.MemoryExtensions.AsMemory(element, start, end).ToArray(), convertToBool)]);
        }
    }

    foreach (var contour in GetPointsFromMemory(Convert(input, start, end, convertToBool)))
    {
        yield return contour;
    }
}
