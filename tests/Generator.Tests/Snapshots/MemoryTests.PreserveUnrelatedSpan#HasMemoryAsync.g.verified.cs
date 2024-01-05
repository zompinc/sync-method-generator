//HintName: Test.Class.HasMemoryAsync.g.cs
public void HasMemory(global::System.Span<byte> mem)
{
    void Inner()
    {
        global::Test.Class.Unrelated mem = new();
        int shouldBeInt = mem.Span;
    }
}
