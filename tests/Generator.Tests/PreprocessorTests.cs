namespace Generator.Tests;

public class PreprocessorTests
{
    [Fact]
    public Task MacrosAroundBraces() => $$"""
#if !BLA
{
#endif

#if !BLA
}
#endif
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task CancellationTokenWrappedByDirective() => """
[CreateSyncVersion]
async Task MethodAsync(StreamReader reader, CancellationToken ct = default)
{
    await reader.ReadLineAsync(
#if NET8_0_OR_GREATER
        ct
#endif
    );
}
""".Verify(true);

    [Fact]
    public Task CancellationTokenWrappedByNegativeDirective() => """
[CreateSyncVersion]
async Task MethodAsync(StreamReader reader, CancellationToken ct = default)
{
    await reader.ReadLineAsync(
#if THIS_IS_FALSE
        ct
#endif
    );
}
""".Verify(true);
}
