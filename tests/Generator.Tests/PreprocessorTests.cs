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

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public Task MethodWrappedByDirective(bool withConfigureAwait) => $$"""
[CreateSyncVersion]
async Task MethodAsync(XmlReader reader, CancellationToken ct = default)
{
    if (await reader.ReadAsync()
#if NET8_0_OR_GREATER
                    .WaitAsync(ct)
#endif
                    {{(withConfigureAwait ? ".ConfigureAwait(false)" : string.Empty)}})
    {
    }
}
""".Verify(true, true);

    [Fact]
    public Task PreprocessorBeforeMethod() => """
#if !THIS_IS_FALSE
private static readonly bool b = false;
#else
private static readonly bool b = false;
#endif

[CreateSyncVersion]
async Task MethodAsync(StreamWriter writer, CancellationToken ct = default)
{
}
""".Verify();
}
