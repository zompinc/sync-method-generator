namespace Generator.Tests;

public class ArgumentTests
{
    [Fact]
    public Task DropNullArgument() => """
public void ProgressMethodAsync() { }

public async Task ProgressMethodAsync(IProgress<int>? a) => await Task.CompletedTask;

[Zomp.SyncMethodGenerator.CreateSyncVersion]
public async Task CallProgressMethodAsync()
{
    await ProgressMethodAsync(null);
}
""".Verify();

    [Fact]
    public Task DropNamedArgument() => """
public void ProgressMethod(int p1 = 0, int p2 = 0) { }

public async Task ProgressMethodAsync(int p1 = 0, int p2 = 0, IProgress<int>? progress = null) => await Task.CompletedTask;

[Zomp.SyncMethodGenerator.CreateSyncVersion]
public async Task CallProgressMethodAsync()
{
    await ProgressMethodAsync(progress: null);
}
""".Verify();
}
