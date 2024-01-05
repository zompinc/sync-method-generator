//HintName: Test.Class.MakeArray.g.cs
private void MakeArray(byte[] sampleBytes)
{
    global::System.Span<byte> mem = global::System.MemoryExtensions.AsSpan(sampleBytes, 0, 123);
    var arr = mem.ToArray();
}
