namespace Generator.Tests;

[UsesVerify]
public class IntegrationTesting
{
#if NETCOREAPP1_0_OR_GREATER
    [Fact]
    public Task GeneratesSyncMethodCorrectly()
    {
        // The source code to test
        var source = """
using System;
using System.Collections.Generic;
using Zomp.SyncMethodGenerator;
using System.Threading;
using System.Threading.Tasks;

namespace Test
{
    public static partial class EnumerableExtensions
    {
        [Zomp.SyncMethodGenerator.CreateSyncVersion]
        public static async IAsyncEnumerable<double> GetAveragesAsync<T>(this IAsyncEnumerable<T> list, int adjacentCount, [EnumeratorCancellation] CancellationToken ct = default)
            where T : IConvertible
        {
            double total = 0;

            int avgCount = adjacentCount * 2 + 1;

            var meanQueue = new Queue<double>();
            await foreach (var o in list.WithCancellation(ct).ConfigureAwait(false))
            {
                var item = o.ToDouble(null);
                if (meanQueue.Count == avgCount)
                {
                    total -= meanQueue.Dequeue();
                }
                meanQueue.Enqueue(item);
                total += item;

                if (meanQueue.Count == avgCount)
                {
                    yield return total / avgCount;
                }
            }
        }
    }
}
""";

        // Pass the source code to our helper and snapshot test the output
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task WithIAsyncEnumerator()
    {
        // The source code to test
        var source = """
using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using Zomp.SyncMethodGenerator;

namespace Test;

public partial class Stuff
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    async Task EnumeratorTestAsync(IAsyncEnumerable<int> range, CancellationToken ct)
    {
        IAsyncEnumerator<int> e = range.GetAsyncEnumerator(ct);
        try
        {
            while (await e.MoveNextAsync().ConfigureAwait(false)) Console.Write(e.Current + " ");
        }
        finally { if (e != null) await e.DisposeAsync(); }
    }
}
""";

        // Pass the source code to our helper and snapshot test the output
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task CombineTwoLists()
    {
        // The source code to test
        var source = """
using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using Zomp.SyncMethodGenerator;

namespace Test;

public partial class Stuff
{
    [CreateSyncVersion]
    public static async IAsyncEnumerable<(TLeft Left, TRight Right)> CombineAsync<TLeft, TRight>(this IAsyncEnumerable<TLeft> list1, IAsyncEnumerable<TRight> list2, [EnumeratorCancellation] CancellationToken ct = default)
    {
        await using var enumerator2 = list2.GetAsyncEnumerator(ct);
        await foreach (var item in list1.WithCancellation(ct).ConfigureAwait(false))
        {
            if (!(await enumerator2.MoveNextAsync().ConfigureAwait(false)))
            {
                throw new InvalidOperationException("Must have the same size");
            }
            yield return (item, enumerator2.Current);
        }
    }
}
""";

        // Pass the source code to our helper and snapshot test the output
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task ChecksumRead()
    {
        // The source code to test
        var source = """
using System;
using System.IO;
using System.Threading;
using System.Threading.Tasks;
using Zomp.SyncMethodGenerator;

namespace Test;

public partial class Stuff
{
    [CreateSyncVersion]
    static async Task<int> HalfCheckSumAsync(Memory<byte> buffer, Stream stream, CancellationToken ct)
        => (await ChecksumReadAsync(buffer, stream, ct).ConfigureAwait(false)) / 2;

    [CreateSyncVersion]
    static async Task<int> ChecksumReadAsync(Memory<byte> buffer, Stream stream, CancellationToken ct)
    {
        int bytesRead = await stream.ReadAsync(buffer, ct).ConfigureAwait(true);
        return Checksum(buffer.Span.Slice(0, bytesRead));
    }
    static int Checksum(Span<byte> buffer) => 0;
}
""";

        // Pass the source code to our helper and snapshot test the output
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task EagerPreconditionsInAsyncMethod()
    {
        // From: https://devblogs.microsoft.com/premier-developer/dissecting-the-local-functions-in-c-7/#use-case-2-eager-preconditions-in-async-methods
        var source = """
using System;
using System.IO;
using System.Collections.Generic;
using System.Threading.Tasks;
using Zomp.SyncMethodGenerator;

namespace Test;

public partial class Stuff
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public static Task<string> GetAllTextAsync(string fileName)
    {
        // Eager argument validation
        if (string.IsNullOrEmpty(fileName)) throw new ArgumentNullException(nameof(fileName));
        return GetAllTextAsync();

        async Task<string> GetAllTextAsync()
        {
            var result = await File.ReadAllTextAsync(fileName);
            return result;
        }
    }
}
""";

        // Pass the source code to our helper and snapshot test the output
        return TestHelper.Verify(source);
    }
#endif

    [Fact]
    public Task EagerPreconditionInIteratorBlock()
    {
        // Moodified from: https://devblogs.microsoft.com/premier-developer/dissecting-the-local-functions-in-c-7/#use-case-1-eager-preconditions-in-iterator-blocks
        var source = """
using System;
using System.IO;
using System.Threading.Tasks;
using Zomp.SyncMethodGenerator;
using System.Collections.Generic;

namespace Test;

public partial class Stuff
{
    [CreateSyncVersion]
    public static IAsyncEnumerable<string> ReadLineByLineAsync(string fileName)
    {
        if (string.IsNullOrEmpty(fileName)) throw new ArgumentNullException(nameof(fileName));

        return ReadLineByLineImpl();

        async IAsyncEnumerable<string> ReadLineByLineImpl()
        {
            await foreach (var line in CreateAsyncEnumerable())
            {
                yield return line;
            }
        }

        async IAsyncEnumerable<string> CreateAsyncEnumerable()
        {
            foreach (var i in new[] { "a", "b" })
            {
                yield return await Task.FromResult(i);
            }
        }
    }
}
""";

        // Pass the source code to our helper and snapshot test the output
        return TestHelper.Verify(source);
    }
}
