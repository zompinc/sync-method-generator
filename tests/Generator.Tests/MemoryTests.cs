namespace Generator.Tests;

public class MemoryTests
{
#if NETCOREAPP1_0_OR_GREATER
    [Fact]
    public Task MemoryToSpan() => """
[CreateSyncVersion]
private async Task ReadAsMemoryAsync(Stream stream, byte[] sampleBytes, CancellationToken ct = default)
    => await stream.ReadAsync(sampleBytes.AsMemory(0, 123), ct).ConfigureAwait(false);
""".Verify();

    [InlineData(false)]
    [InlineData(true)]
    [Theory]
    public Task MemorySpanProperty(bool explicitType)
    {
        var typeName = explicitType ? "Memory<byte>" : "var";

        return $$"""
[CreateSyncVersion]
private async Task MakeArray(byte[] sampleBytes, CancellationToken ct = default)
{
    {{typeName}} mem = sampleBytes.AsMemory(0, 123);
    var arr = mem.Span.ToArray();
}
""".Verify(false, false, parameters: explicitType);
    }

    [Fact]
    public Task ReadOnlyMemoryToReadOnlySpan() => """
[CreateSyncVersion]
static async Task WriteAsync(ReadOnlyMemory<byte> buffer, Stream stream, CancellationToken ct)
    => await stream.WriteAsync(buffer, ct).ConfigureAwait(true);
""".Verify();
#endif

    [Fact]
    public Task DropSpanAfterAsMemory() => """
[CreateSyncVersion]
public async Task MakeArrayAsync(byte[] sampleBytes, CancellationToken ct = default)
{
    var arr = sampleBytes.AsMemory(0, 123).Span.ToArray();
}
""".Verify();

    [Fact]
    public Task PreserveUnrelatedSpan() => """
[CreateSyncVersion]
public async Task HasMemoryAsync(Memory<byte> mem)
{
    void Inner()
    {
        Unrelated mem = new();
        int shouldBeInt = mem.Span;
    }
}

class Unrelated
{
    public int Span { get; set; }
}
""".Verify();

    [Fact]
    public Task NonPredefinedTypes() => """
using N2;
using static N2.C1;

namespace N1
{
    partial class Class
    {
        [CreateSyncVersion]
        async Task FillAsync(Memory<C2.Accelerometer> accelerometers)
        {
            C2.Accelerometer a = new(1, 2, 3);
            accelerometers.Span[0] = a;
        }
    }
}

namespace N2
{
    internal class C1
    {
        public class C2
        {
            public record struct Accelerometer(short X, short Y, short Z);
        }
    }
}
""".Verify(sourceType: SourceType.Full);
}
