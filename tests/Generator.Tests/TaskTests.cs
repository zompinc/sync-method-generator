namespace Generator.Tests;

public class TaskTests
{
#if NET6_0_OR_GREATER
    [Fact]
    public Task DropWaitAsync() => """
[CreateSyncVersion]
public async Task MethodAsync(XmlReader reader, CancellationToken ct)
{
    _ = await reader.ReadAsync().WaitAsync(ct);
}
""".Verify();

    [Fact]
    public Task DropWaitAsyncStatement() => """
[CreateSyncVersion]
public async Task MethodAsync(Task task, CancellationToken ct)
{
    await task.WaitAsync(ct);
}
""".Verify();
#endif

    [Fact]
    public Task DropConfigureAwaitStatement() => """
[CreateSyncVersion]
public async Task MethodAsync(Task task, CancellationToken ct)
{
    await task.ConfigureAwait(false);
}
""".Verify();
}
