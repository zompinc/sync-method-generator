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
}
