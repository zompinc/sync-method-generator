using System;
using System.Threading.Tasks;

namespace GenerationSandbox.Tests;

internal sealed partial class AsyncWithIProgress
{
    public static async Task WithIProgressAsync(IProgress<float>? progress = null)
    {
        await Task.CompletedTask;
    }

    public static void WithIProgress()
    {
    }

    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public static async Task CallWithIProgressAsync()
    {
        CustomProgress progress = new();

        if (true)
        {
            progress++;
        }

        await WithIProgressAsync(progress);
    }

    private sealed class CustomProgress : IProgress<float>
    {
        public static CustomProgress operator ++(CustomProgress a) => a;

        public void Report(float value) => throw new NotImplementedException();
    }
}
