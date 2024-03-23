using System;
using System.Threading;
using System.Threading.Tasks;

namespace GenerationSandbox.Tests;

internal static partial class WhileNotCancelled
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public static async ValueTask SleepAsync(CancellationToken ct)
    {
        while (!ct.IsCancellationRequested)
        {
            await Task.Delay(120000, ct);
        }

        throw new OperationCanceledException();
    }
}
