//HintName: Test.Class.GetMemoryOrSpanAsync.g.cs
private void GetMemoryOrSpan()
{
    var instance = new global::Test.Class.ClassThatReturnsMemoryAndSpan();
    global::System.Span<byte> mem = instance.GetSpan();
    var arr = mem.ToArray();
}
