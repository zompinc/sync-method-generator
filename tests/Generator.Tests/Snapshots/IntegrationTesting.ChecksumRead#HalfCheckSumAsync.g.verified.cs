//HintName: Test.Class.HalfCheckSumAsync.g.cs
static int HalfCheckSum(global::System.Span<byte> buffer, global::System.IO.Stream stream)
    => ChecksumRead(buffer, stream) / 2;
