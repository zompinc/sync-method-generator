using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;

namespace GenerationSandbox.Tests;

public static partial class AsyncExtensions
{
    /// <summary>
    /// Calculates rolling average
    /// </summary>
    /// <param name="source">Items to average</param>
    /// <param name="windowSize">Size of averaging window</param>
    /// <param name="progress">Reports progress</param>
    /// <param name="ct">Cancellation token</param>
    /// <returns>Moving averages</returns>
    /// <remarks>
    /// Credit: modified from https://www.codeguru.co.in/2021/06/moving-average-in-c-using-linq.html
    /// </remarks>
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public static async IAsyncEnumerable<double> MovingAverageAsync(this IAsyncEnumerable<double> source, int windowSize, IProgress<int>? progress = null, [EnumeratorCancellation] CancellationToken ct = default)
    {
        var queue = new Queue<double>(windowSize);

        var i = 0;
        await foreach (double d in source)
        {
            ct.ThrowIfCancellationRequested();
            if ((i & 0x3) == 0)  // report progress every 4 iterations
            {
                progress?.Report(i);
            }
            if (queue.Count == windowSize)
            {
                queue.Dequeue();
            }
            queue.Enqueue(d);
            yield return queue.Average();
            ++i;
        }
    }
}
