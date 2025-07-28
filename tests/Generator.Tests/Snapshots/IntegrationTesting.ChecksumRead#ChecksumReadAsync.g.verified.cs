//HintName: Test.Class.ChecksumReadAsync.g.cs
static int ChecksumRead(global::System.Span<byte> buffer, global::System.IO.Stream stream)
{
    int bytesRead = stream.Read(buffer);
    return global::Test.Class.Checksum(buffer.Slice(0, bytesRead));
}
