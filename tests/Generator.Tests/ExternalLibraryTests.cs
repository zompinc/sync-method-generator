namespace Generator.Tests;

public class ExternalLibraryTests
{
#if NETCOREAPP1_0_OR_GREATER
    [Fact]
    public Task ToListAsync() => """
[CreateSyncVersion]
public async Task MethodAsync(IAsyncEnumerable<int> @enum)
{
    _ = await @enum.ToListAsync();
}
""".Verify();
#endif
}
