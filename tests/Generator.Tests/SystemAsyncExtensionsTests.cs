namespace Generator.Tests;

public class SystemAsyncExtensionsTests
{
    [Fact]
    public Task DropConfiguredCancelableAsyncEnumerable() => """
[CreateSyncVersion]
public static async Task Iterate<T>(this IAsyncEnumerable<T> list1, [EnumeratorCancellation] CancellationToken ct = default)
{
    await foreach (var item in list1.ConfigureAwait(false))
    {
    }
}
""".Verify();

    [Fact]
    public Task DropWithCancellation() => """
[CreateSyncVersion]
public static async Task Iterate<T>(this IAsyncEnumerable<T> list1, [EnumeratorCancellation] CancellationToken ct = default)
{
    await foreach (var item in list1.WithCancellation(ct))
    {
    }
}
""".Verify();

    [Theory]
    [InlineData(true, true)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(false, false)]
    public Task DropConfigureAwaitExtensions(bool isGeneric, bool isValueTask)
    {
        var taskPart = isValueTask ? "ValueTask" : "Task";
        var genericPart = isGeneric ? "<T>" : string.Empty;
        var genericTaskPart = taskPart + genericPart;
        var variablePart = isGeneric ? "T" : "int";
        return $$"""
namespace Test;

public static class AwaitHelper
{
    public static Configured{{taskPart}}Awaitable{{genericPart}} Caf{{genericPart}}(this {{genericTaskPart}} task)
    {
        return task.ConfigureAwait(false);
    }
}

partial class Class
{
    {{variablePart}} GetItem{{genericPart}}({{variablePart}} z) => default;

    async {{genericTaskPart}} GetItemAsync{{genericPart}}({{variablePart}} z) => default;

    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public async Task InvokeAsync()
        => _ = await GetItemAsync(4).Caf();
}
""".Verify(disableUnique: true, sourceType: SourceType.Full);
    }

    [Fact]
    public Task AsyncForEachQualified() => """
[CreateSyncVersion]
async Task<int> SumAsync(IAsyncEnumerable<int?> enumerable)
{
    int sum = 0;

    await foreach (int? i in enumerable)
    {
        if (i.HasValue)
        {
            sum += i.Value;
        }
    }
    
    return sum;
}
""".Verify();

    [Fact]
    public Task AsyncForEachDeconstruct() => """
[CreateSyncVersion]
async Task<int> SumAsync(IAsyncEnumerable<(int a, int b)> enumerable)
{
    int sum = 0;

    await foreach (var (a, b) in enumerable)
    {
        sum += a + b;
    }
    
    return sum;
}
""".Verify();
}
