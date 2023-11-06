//HintName: Test.Class.ReadLineByLineAsync.g.cs
public static global::System.Collections.Generic.IEnumerable<string> ReadLineByLine(string fileName)
{
    if (string.IsNullOrEmpty(fileName)) throw new global::System.ArgumentNullException(nameof(fileName));

    return ReadLineByLineImpl();

    global::System.Collections.Generic.IEnumerable<string> ReadLineByLineImpl()
    {
        foreach (var line in CreateAsyncEnumerable())
        {
            yield return line;
        }
    }

    global::System.Collections.Generic.IEnumerable<string> CreateAsyncEnumerable()
    {
        foreach (var i in new[] { "a", "b" })
        {
            yield return i;
        }
    }
}
