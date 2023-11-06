namespace Generator.Tests;

[UsesVerify]
public class GenericMathTests
{
    [Fact]
    public Task GeneratesWithIProgressCorrectly()
    {
        // The source code to test
        var source = $$"""
using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
#if NET7_0_OR_GREATER
using System.Numerics;
#endif
using Zomp.SyncMethodGenerator;

namespace Test
{
    public static partial class EnumerableExtensions
    {
        /// <summary>
        /// Finds maximum value so far and returns its 0 based index
        /// </summary>
        /// <typeparam name="T">Numeric type</typeparam>
        /// <param name="items">Items to scan.</param>
        /// <param name="progress">The progress</param>
        /// <param name="ct">Cancellation token</param>
        /// <returns>Max indices</returns>
        [Zomp.SyncMethodGenerator.CreateSyncVersion]
        public static async IAsyncEnumerable<int> IndexOfMaxSoFarAsync<T>(this IAsyncEnumerable<T> items, IProgress<int>? progress = null, [EnumeratorCancellation] CancellationToken ct = default)
#if NET7_0_OR_GREATER
            where T : IComparisonOperators<T, T, bool>
#else
            where T : IComparisonOperators<T, T>
#endif
        {
            var i = 0;

            T? largestSoFar = default;
            await foreach (var item in items)
            {
                ct.ThrowIfCancellationRequested();

                if ((i & 0x3ff) == 0)
                {
                    progress?.Report(i);
                }

                if (largestSoFar is null || largestSoFar < item)
                {
                    largestSoFar = item;
                    yield return i;
                }

                ++i;
            }
        }
    }
}
""";

        // Pass the source code to our helper and snapshot test the output
        return TestHelper.Verify(source, true, isFullSource: true);
    }
}
