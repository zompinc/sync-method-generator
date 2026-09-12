namespace Generator.Tests;

public class IsCancellationRequestedTests
{
    [Fact]
    public Task WhileNotCancelled() => $$"""
while (((!((ct.IsCancellationRequested)))))
{
    await Task.Delay(120000, ct);
}
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task IfCancelled() => $$"""
if (((((ct.IsCancellationRequested)))))
{
    await Task.Delay(120000, ct);
}
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task IfNotCancelled() => $$"""
if (((!((ct.IsCancellationRequested)))))
{
    await Task.Delay(120000, ct);
}
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task WhileNotCancelledThenThrow() => """
while (!ct.IsCancellationRequested)
{
    await Task.Delay(120000, ct);
}

throw new OperationCanceledException();
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task WhileNotCancelledChecksInside() => """
while (!ct.IsCancellationRequested)
{
    await Task.Delay(120000, ct);
    if (ct.IsCancellationRequested)
    {
        break;
    }
}

Console.WriteLine("Stopped");
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task WhileNotCancelledReturns() => """
while (!ct.IsCancellationRequested)
{
    await Task.Delay(120000, ct);
    if (Environment.TickCount > 0)
    {
        return;
    }
}

Console.WriteLine("Stopped");
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task WhileNotCancelledBreaks() => """
while (!ct.IsCancellationRequested)
{
    await Task.Delay(120000, ct);
    if (Environment.TickCount > 0)
    {
        break;
    }
}

Console.WriteLine("Stopped");
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task BreakInsideSwitchStaysInLoop() => """
while (!ct.IsCancellationRequested)
{
    await Task.Delay(120000, ct);
    switch (Environment.TickCount)
    {
        case 0:
            break;
    }
}
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task WhileNotCancelledNeverReturns() => """
[CreateSyncVersion]
async Task MethodAsync(CancellationToken ct)
{
    while (!ct.IsCancellationRequested)
    {
        await Task.Delay(120000, ct);
        if (Environment.TickCount > 0)
        {
            Fail();
        }
    }
}

[System.Diagnostics.CodeAnalysis.DoesNotReturn]
static void Fail() => throw new InvalidOperationException();
""".Verify();

    [Fact]
    public Task WhileTrueWrittenByHand() => """
while (true)
{
    await Task.Delay(120000, ct);
}
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task WhileNotCancelledReplacedBySyncOnly() => """
#if SYNC_ONLY
System.Threading.Thread.Sleep(120000);
#else
while (!ct.IsCancellationRequested)
{
    await Task.Delay(120000, ct);
}
#endif
""".Verify(sourceType: SourceType.MethodBody);
}
