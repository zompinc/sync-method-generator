namespace Generator.Tests;

public class ForEachTests
{
    [Fact]
    public Task AsyncForEachQualified() => """
[CreateSyncVersion]
async Task<int> SumAsync(IAsyncEnumerable<int?> enumerable)
{
    int sum = 0;

    await foreach (int? i in enumerable)
    {
        if (i.HasValue)
        {
            sum += i.Value;
        }
    }
    
    return sum;
}
""".Verify();

    [Fact]
    public Task AsyncForEachDeconstruct() => """
[CreateSyncVersion]
async Task<int> SumAsync(IAsyncEnumerable<(int a, int b)> enumerable)
{
    int sum = 0;

    await foreach (var (a, b) in enumerable)
    {
        sum += a + b;
    }
    
    return sum;
}
""".Verify();
}
