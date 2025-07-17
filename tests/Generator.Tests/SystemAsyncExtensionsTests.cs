namespace Generator.Tests;

public class SystemAsyncExtensionsTests
{
    private const string ConfiguredCancelableAsyncEnumerableDefinition = """
    // From https://github.com/dotnet/reactive/blob/5f831de0bc70bc660d21c3b9e04e581269691a2a/Ix.NET/Source/System.Linq.Async/System/Threading/Tasks/AsyncEnumerableExt.cs#L15
    internal static ConfiguredCancelableAsyncEnumerable<T>.Enumerator GetConfiguredAsyncEnumerator<T>(this IAsyncEnumerable<T> enumerable, CancellationToken ct)
    {
        return enumerable.ConfigureAwait(false).WithCancellation(ct).GetAsyncEnumerator();
    }
""";

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

    [Fact]
    public Task ConfiguredCancelableAsyncEnumerable() => """
[CreateSyncVersion]
internal async Task<int> MethodAsync(IAsyncEnumerable<(Stream A, int B)> enumerable, CancellationToken ct)
{
    var enumerator = enumerable.WithCancellation(ct).ConfigureAwait(false).GetAsyncEnumerator();

    int sum = 0;
    while (await enumerator.MoveNextAsync())
    {
        sum += (int)enumerator.Current.A.Length + enumerator.Current.B;
    }

    return sum;
}
""".Verify();

    [Fact]
    public Task ConfiguredCancelableAsyncEnumerableExtension() => """
// From https://github.com/dotnet/reactive/blob/5f831de0bc70bc660d21c3b9e04e581269691a2a/Ix.NET/Source/System.Linq.Async/System/Threading/Tasks/AsyncEnumerableExt.cs#L15

[CreateSyncVersion]
public static ConfiguredCancelableAsyncEnumerable<T>.Enumerator GetConfiguredEnumeratorAsync<T>(IAsyncEnumerable<T> enumerable, CancellationToken ct)
{
    return enumerable.ConfigureAwait(false).WithCancellation(ct).GetAsyncEnumerator();
}
""".Verify();

    [Fact]
    public Task ConfiguredCancelableAsyncEnumerableExtensionUsingStatic() => """
using static System.Runtime.CompilerServices.ConfiguredCancelableAsyncEnumerable<int>;
namespace Test;

internal static partial class Extensions
{
    [CreateSyncVersion]
    public static Enumerator GetConfiguredAsyncEnumerator(IAsyncEnumerable<int> enumerable, CancellationToken ct)
    {
        return enumerable.ConfigureAwait(false).WithCancellation(ct).GetAsyncEnumerator();
    }
}
""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task DropConfiguredCancelableAsyncEnumerableExtensionInvocation() => $$"""
namespace Test;

internal static partial class Extensions
{
{{ConfiguredCancelableAsyncEnumerableDefinition}}
    [CreateSyncVersion]
    internal static async Task MethodAsync(IAsyncEnumerable<int> enumerable, CancellationToken ct)
    {
        await using ConfiguredCancelableAsyncEnumerable<int>.Enumerator enumerator = enumerable.GetConfiguredAsyncEnumerator(ct);
    }
}
""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task DisposeConfiguredCancelableAsyncEnumerator() => $$"""
namespace Test;

internal static partial class Extensions
{
{{ConfiguredCancelableAsyncEnumerableDefinition}}

    [CreateSyncVersion]
    internal static async Task MethodAsync(IAsyncEnumerable<int> enumerable, CancellationToken ct)
    {
        var enumerator = enumerable.GetConfiguredAsyncEnumerator(ct);
        await enumerator.DisposeAsync();
    }
}
""".Verify(sourceType: SourceType.Full);
}
