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
}
