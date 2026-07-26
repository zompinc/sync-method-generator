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
    public Task DropWaitAsyncFullSource() => """
private XmlReader reader;

[CreateSyncVersion]
public async Task MethodAsync(CancellationToken ct = default)
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

    [Fact]
    public Task ReportWhenAll() => """
[CreateSyncVersion]
public async Task MethodAsync()
{
    await Task.WhenAll(FooAsync(), FooAsync());
}

private async Task FooAsync() => await Task.CompletedTask;
private void Foo() { }
""".Verify();

    [Fact]
    public Task ReportWhenAny() => """
[CreateSyncVersion]
public async Task MethodAsync()
{
    _ = await Task.WhenAny(FooAsync(), FooAsync());
}

private async Task FooAsync() => await Task.CompletedTask;
private void Foo() { }
""".Verify();
}
