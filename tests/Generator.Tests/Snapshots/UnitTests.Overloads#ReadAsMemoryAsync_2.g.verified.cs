//HintName: Test.Class.ReadAsMemoryAsync_2.g.cs
void ReadAsMemory(global::System.IO.Stream stream, byte[] sampleBytes)
    => stream.Read(global::System.MemoryExtensions.AsSpan(sampleBytes, 0, 123));
