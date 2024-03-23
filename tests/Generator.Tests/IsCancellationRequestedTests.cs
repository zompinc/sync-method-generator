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
    public Task WhileNotCancelledThrow() => $$"""
while (!ct.IsCancellationRequested)
{
    await Task.Delay(120000, ct);
}

throw new OperationCanceledException();
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task WhileNotCancelledBreakThrow() => $$"""
while (!ct.IsCancellationRequested)
{
    await Task.Delay(120000, ct);
    break;
}

throw new OperationCanceledException();
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task WhileNotCancelledInvalidBreakThrow() => $$"""
while (!ct.IsCancellationRequested)
{
    await Task.Delay(120000, ct);
    while (true) break;
}

throw new OperationCanceledException();
""".Verify(sourceType: SourceType.MethodBody);

#if NET6_0_OR_GREATER

    [Fact]
    public Task WhileCancelled() => $$"""
[System.Diagnostics.CodeAnalysis.DoesNotReturn]
public void ThrowError() => throw new Exception();

[Zomp.SyncMethodGenerator.CreateSyncVersion]
public async Task CallProgressMethodAsync(CancellationToken ct)
{
    while (!ct.IsCancellationRequested)
    {
        ThrowError();
    }
}
""".Verify();

#endif
}
