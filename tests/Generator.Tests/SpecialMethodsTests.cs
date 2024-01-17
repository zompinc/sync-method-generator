namespace Generator.Tests;

public class SpecialMethodsTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public Task TaskDelayToThreadSleepWithInt(bool caf) =>
        $"await Task.Delay(1){Caf(caf)};"
        .Verify(disableUnique: true, sourceType: SourceType.MethodBody);

    [Fact]
    public Task TaskDelayToThreadSleepWithSpan() =>
        "await Task.Delay(new TimeSpan(2, 3, 4));"
        .Verify(sourceType: SourceType.MethodBody);

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public Task FromTaskResultCheck(bool caf) =>
        $"_ = await Task.FromResult(1){Caf(caf)};"
        .Verify(disableUnique: true, sourceType: SourceType.MethodBody);

#if NETCOREAPP1_0_OR_GREATER
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public Task FromValueTaskResultCheck(bool caf) =>
        $"_ = await ValueTask.FromResult(1){Caf(caf)};"
        .Verify(disableUnique: true, sourceType: SourceType.MethodBody);
#endif

    [Theory]
    [InlineData("await PrivateClass.Delay(2, 5);")]
    [InlineData("var z = await PrivateClass.FromResult(2, 6);")]
    public Task DropMethodsWithSpecialNamesInUserClass(string invocation) => $$"""
[CreateSyncVersion]
public static async Task<int> MethodAsync(CancellationToken ct)
{
    {{invocation}}
}

private class PrivateClass
{
    public static Task Delay(int delay, int unrelated) => Task.CompletedTask;

    public static Task<TResult> FromResult<TResult>(TResult delay, int unrelated) => Task.FromResult(delay);
}
""".Verify(disableUnique: true);

    private static string Caf(bool apply) => apply ? ".ConfigureAwait(false)" : string.Empty;
}
