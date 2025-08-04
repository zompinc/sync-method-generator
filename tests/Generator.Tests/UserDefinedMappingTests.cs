using Generator.Tests.Utils;

namespace Generator.Tests;

public class UserDefinedMappingTests
{
    [Fact]
    public Task Extension() => """
using System.Linq;

namespace Test;

internal static class AsyncQueryable
{
    public static async Task<List<TSource>> ToListAsync<TSource>(this IQueryable<TSource> source) => throw new NotImplementedException();
}

internal partial class Class<T>
{
    [CreateSyncVersion]
    public async Task FooAsync(IQueryable<string> source)
    {
        source.ToListAsync();
    }
}
""".Verify(sourceType: SourceType.Full, additionalTexts: [
    new MemoryAdditionalText("SyncMethods.txt", "Test.AsyncQueryable.ToListAsync=System.Linq.Enumerable.ToList")
]);

    [Fact]
    public Task ExtensionDirectCall() => """
using System.Linq;

namespace Test;

internal static class AsyncQueryable
{
    public static async Task<List<TSource>> ToListAsync<TSource>(this IQueryable<TSource> source) => throw new NotImplementedException();
}

internal partial class Class<T>
{
    [CreateSyncVersion]
    public async Task FooAsync(IQueryable<string> source)
    {
        Test.AsyncQueryable.ToListAsync(source);
    }
}
""".Verify(sourceType: SourceType.Full, additionalTexts: [
    new MemoryAdditionalText("SyncMethods.txt", "Test.AsyncQueryable.ToListAsync=System.Linq.Enumerable.ToList")
]);

    [Fact]
    public Task OverrideSpecialMethod() => """
using System.Threading.Tasks;

namespace Test;

internal static class CustomThread
{
    public void Sleep(int millisecondsTimeout) => throw new NotImplementedException();
}

internal partial class Class<T>
{
    [CreateSyncVersion]
    public async async Task FooAsync()
    {
       await Task.Delay(1000);
    }
}
""".Verify(sourceType: SourceType.Full, additionalTexts: [
    new MemoryAdditionalText("SyncMethods.txt", "System.Threading.Tasks.Task.Delay=Test.CustomThread.Sleep")
]);

    [Fact]
    public Task DuplicateKey() => string.Empty.Verify(sourceType: SourceType.Full, additionalTexts: [
    new MemoryAdditionalText("SyncMethods.txt", """
System.Threading.Tasks.Task.Delay=Test.CustomThread.Sleep
System.Threading.Tasks.Task.Delay=Test.CustomThread.Sleep
""")
]);

    [Fact]
    public Task SelfCall() => """
public Task Delay(int millisecondsTimeout) => Task.Delay(millisecondsTimeout);

[CreateSyncVersion]
public async async Task FooAsync()
{
   await Delay(1000);
}
""".Verify(additionalTexts: [
    new MemoryAdditionalText("SyncMethods.txt", "Test.Class.Delay=System.Threading.Thread.Sleep")
]);

    [Fact]
    public Task ErrorOnUserMapping() => """
[CreateSyncVersion]
public Task Delay(int millisecondsTimeout) => Task.Delay(millisecondsTimeout);

[CreateSyncVersion]
public async async Task FooAsync()
{
   await Delay(1000);
}
""".Verify(additionalTexts: [
    new MemoryAdditionalText("SyncMethods.txt", "Test.Class.Delay=System.Threading.Thread.Sleep")
]);
}
