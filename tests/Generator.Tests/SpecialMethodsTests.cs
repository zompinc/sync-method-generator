namespace Generator.Tests;

[UsesVerify]
public class SpecialMethodsTests
{
    [Theory]
    [InlineData("")]
    [InlineData(".ConfigureAwait(false)")]
    public Task TaskDelayToThreadSleepWithInt(string extend) =>
        $"await Task.Delay(1){extend};"
        .Verify(disableUnique: true, sourceType: SourceType.MethodBody);

    [Fact]
    public Task TaskDelayToThreadSleepWithSpan() =>
        "await Task.Delay(new TimeSpan(2, 3, 4));"
        .Verify(sourceType: SourceType.MethodBody);

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
}
