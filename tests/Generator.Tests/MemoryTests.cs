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
    public Task DropSpanAfterAsMemoryWithWhitespace() => """
[CreateSyncVersion]
public async Task MakeArrayAsync(byte[] sampleBytes, CancellationToken ct = default)
{
    var arr =
        sampleBytes.AsMemory(0, 123).Span.ToArray();
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

    [Fact]
    public Task ConvertMemoryToSpan() => """
Memory<byte> GetMemory() => MemoryPool<byte>.Shared.Rent().Memory;

[CreateSyncVersion]
async Task MethodAsync()
{
    var m = GetMemory();
}
""".Verify();

    [Fact]
    public Task ConvertMemoryToSpanAfterPropertyAccess() => """
var m = MemoryPool<byte>.Shared.Rent().Memory;
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task ListOfMemories() => """
[CreateSyncVersion]
public static async Task WriteAsync(IAsyncEnumerable<ReadOnlyMemory<byte>> buffers, Stream stream, CancellationToken ct)
{
    await foreach (var buffer in buffers)
    {
        var length = buffer.Span.Length;
        await stream.WriteAsync(buffer, ct).ConfigureAwait(true);
    }
}
""".Verify();

    [Fact]
    public Task ChangeType() => """

public static async IEnumerable<System.Drawing.Point> GetPointsFromMemory(IEnumerable<ReadOnlyMemory<bool>> input)
{
    yield return default;
}

public static async IAsyncEnumerable<System.Drawing.Point> GetPointsFromMemoryAsync(IAsyncEnumerable<ReadOnlyMemory<bool>> input, [EnumeratorCancellation] CancellationToken cancellationToken = default)
{
    yield return default;
}

[CreateSyncVersion]
public static async IAsyncEnumerable<Point> GetPoints<T>(IAsyncEnumerable<T[]> input, int start, int end, Func<T, bool> convertToBool, [EnumeratorCancellation] CancellationToken cancellationToken = default)
{
    static async IAsyncEnumerable<ReadOnlyMemory<bool>> Convert(IAsyncEnumerable<T[]> input, int start, int end, Func<T, bool> convertToBool, [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        await foreach (var element in input.WithCancellation(cancellationToken).ConfigureAwait(false))
        {
            yield return new ReadOnlyMemory<bool>([.. element.AsMemory(start, end).ToArray().Select(convertToBool)]);
        }
    }

    await foreach (var contour in GetPointsFromMemoryAsync(Convert(input, start, end, convertToBool, cancellationToken), cancellationToken).ConfigureAwait(false))
    {
        yield return contour;
    }
}
""".Verify();

    [Fact]
    public Task AccessIndexer() => """
[CreateSyncVersion]
public static async IAsyncEnumerable<Memory<bool>> MethodAsync<T>(IAsyncEnumerable<Memory<bool>> input, [EnumeratorCancellation] CancellationToken cancellationToken = default)
{
    await foreach (var elem in input.ConfigureAwait(false))
    {
        _ = elem.Span[0];
        yield return elem;
    }
}
""".Verify();
}
