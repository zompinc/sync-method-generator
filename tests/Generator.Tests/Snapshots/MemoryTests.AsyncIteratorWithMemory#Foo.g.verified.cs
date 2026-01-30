//HintName: Test.Class.Foo.g.cs
public static global::System.Collections.Generic.IEnumerable<int> Foo(
    global::System.Collections.Generic.IEnumerable<global::System.ReadOnlyMemory<bool>> input)
{
    global::System.ReadOnlyMemory<bool> prev = default;
    var hasPrev = false;

    foreach (var col in input)
    {
        if (hasPrev)
        {
            global::Test.Class.Helper(prev.Span, col.Span);
        }

        prev = col;
        hasPrev = true;

        yield return 1;
    }
}
