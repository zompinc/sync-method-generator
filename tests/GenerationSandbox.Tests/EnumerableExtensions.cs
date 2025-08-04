using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace GenerationSandbox.Tests;

internal static partial class EnumerableExtensions
{
    public static Task<List<TSource>> ToListAsync<TSource>(this IEnumerable<TSource> source)
    {
        return Task.FromResult(source.ToList());
    }

    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public static Task<List<TSource>> ReturnListAsync<TSource>(IEnumerable<TSource> source)
    {
        return ToListAsync(source);
    }
}
