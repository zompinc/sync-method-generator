//HintName: Test.Class.MethodAsync.g.cs
public static global::System.Collections.Generic.IEnumerable<global::System.Memory<bool>> Method<T>(global::System.Collections.Generic.IEnumerable<global::System.Memory<bool>> input)
{
    foreach (var elem in input)
    {
        _ = elem.Span[0];
        yield return elem;
    }
}
