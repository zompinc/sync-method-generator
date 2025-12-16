namespace Generator.Tests;

public class ExtensionMethodTests
{
    [Fact]
    public Task UnwrapGenericExtensionMethod() => """
using System.Drawing;
using System.Threading;
using System.Threading.Tasks;

namespace Zomp.SyncMethodGenerator.IntegrationTests
{
    using Extensi.ons123;
    partial class Extensions
    {
        [CreateSyncVersion]
        public static async Task HasGenericExtensionAsync(object o, CancellationToken ct)
        {
            var z = o.TryGetValue<Point>(out var item);
        }

        [CreateSyncVersion]
        public static async Task HasGeneric2ExtensionAsync(object o, CancellationToken ct)
        {
            var z = o.TryGetValue<Point, PointF>(out var _, out var _1);
        }
    }
}

namespace Extensi.ons123
{
    internal static class MyExtensionClass
    {
        public static bool TryGetValue<T>(this object _, out T? item)
        {
            item = default;
            return false;
        }

        public static bool TryGetValue<T1, T2>(this object _, out T1? item1, out T2? item2)
        {
            item1 = default;
            item2 = default;
            return false;
        }
    }
}
""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task LeftOfTheDotTest() => """
namespace Tests;

internal class Bar
{
    public static Bar Create() => new Bar();
}

partial class Class
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public async Task MethodAsync()
    {
        _ = Bar.Create()?.DoSomething();
    }
}

internal static class BarExtension
{
    public static Bar DoSomething(this Bar bar) => bar;
}
""".Verify(sourceType: SourceType.Full);

#if NET8_0_OR_GREATER
    [Fact]
    public Task CSharp_14_Extensions() => """
namespace Tests;

static partial class Class
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public async IAsyncEnumerable<T> WhereGreaterThan(this IAsyncEnumerable<T> source, T threshold, int dummy)
    {
        await foreach (var item in source)
        {
            if (item > threshold)
            {
                yield return item;
            }
        }
    }

    extension<T>(IAsyncEnumerable<T> source)
        where T : INumber<T>
    {
        [Zomp.SyncMethodGenerator.CreateSyncVersion]
        public async IAsyncEnumerable<T> WhereGreaterThan(T threshold)
        {
            await foreach (var item in source)
            {
                if (item > threshold)
                {
                    yield return item;
                }
            }
        }
        [Zomp.SyncMethodGenerator.CreateSyncVersion]
        public async IAsyncEnumerable<T> WhereLessThan(T threshold)
        {
            await foreach (var item in source)
            {
                if (item < threshold)
                {
                    yield return item;
                }
            }
        }
    }
}
""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task EntityFrameworkQueryableExtensions() => """
using System.Threading;
using System.Threading.Tasks;

namespace Zomp.SyncMethodGenerator.IntegrationTests
{
    using Microsoft.EntityFrameworkCore;

    public partial class EntityFrameworkQueryableExtensions
    {
        [Zomp.SyncMethodGenerator.CreateSyncVersion]
        public async Task<int> QueryableExtensionAsync(DbContext dbContext, CancellationToken cancellationToken)
        {
            var dbSet = dbContext.Set<object>();
            if (await dbSet.AnyAsync(cancellationToken))
            {
                return await dbSet.ExecuteDeleteAsync(cancellationToken);
            }
            
            return 0;
        }
    }
}
""".Verify(sourceType: SourceType.Full);
#endif
}
