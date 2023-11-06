//HintName: Test.Class.MakeArray.g.cs
private void MakeArray(byte[] sampleBytes)
{
    var mem = global::System.MemoryExtensions.AsSpan(sampleBytes, 0, 123);
    var arr = mem.ToArray();
}
