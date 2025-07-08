//HintName: Test.Class.WriteAsync.g.cs
public static void Write(global::System.Collections.Generic.IEnumerable<global::System.ReadOnlyMemory<byte>> buffers, global::System.IO.Stream stream)
{
    foreach (var buffer in buffers)
    {
        var length = buffer.Span.Length;
        stream.Write(buffer.Span);
    }
}
