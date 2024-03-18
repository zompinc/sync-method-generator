//HintName: Test.Class.MakeArrayAsync.g.cs
public void MakeArray(byte[] sampleBytes)
{
    var arr =
        global::System.MemoryExtensions.AsSpan(sampleBytes, 0, 123).ToArray();
}
