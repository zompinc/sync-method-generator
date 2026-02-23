namespace Generator.Tests;

public class PreserveCancellationTokenTests
{
    [Fact]
    public Task PreserveIsCancellationRequestedAndThrowIfCancellationRequested() => """
[CreateSyncVersion(PreserveCancellationToken = true)]
async Task MethodAsync(CancellationToken ct = default)
{
    if (!ct.IsCancellationRequested)
    {
        ct.ThrowIfCancellationRequested();
    }
}
""".Verify();

    [Fact]
    public Task RemoveCancellationTokenForSyncMethods() => """
[CreateSyncVersion(PreserveCancellationToken = true)]
async Task MethodAsync(CancellationToken ct = default)
{
    Stream.Null.FlushAsync(ct);
}
""".Verify();

    [Theory]
    [InlineData("[EnumeratorCancellation]")]
    [InlineData("[EnumeratorCancellationAttribute]")]
    [InlineData("[System.Runtime.CompilerServices.EnumeratorCancellation]")]
    [InlineData("[System.Runtime.CompilerServices.EnumeratorCancellationAttribute]")]
    [InlineData("[global::System.Runtime.CompilerServices.EnumeratorCancellationAttribute]")]
    public Task RemoveEnumeratorCancellationAttribute(string attribute) => $$"""
[CreateSyncVersion(PreserveCancellationToken = true)]
async IAsyncEnumerable<int> FibonacciAsync({{attribute}} CancellationToken ct = default)
{
    var f0 = 0;
    var f1 = 1;
    yield return f0;
    yield return f1;
    while (!ct.IsCancellationRequested)
    {
        var fn = f0 + f1;
        yield return fn;
        await Task.Yield();
        f0 = f1;
        f1 = fn;
    }
}
""".Verify(disableUnique: true);
}
