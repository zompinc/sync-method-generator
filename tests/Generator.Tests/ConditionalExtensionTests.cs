namespace Generator.Tests;

public class ConditionalExtensionTests
{
    [Fact]
    public Task NullConditionalWithoutExtension() => """
Assembly? a = null;
_ = a?.ToString();
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task NullConditionalExtension() => """
Assembly? a = null;
_ = a?.GetCustomAttributes();
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task StartWithExpression() => """
_ = GetType().Assembly?.GetCustomAttributes();
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task NullConditionalExtensionChained() => """
Assembly? a = null;
_ = a?.GetCustomAttributes()?.ToList();
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task LongChained() => """
Assembly? a = null;
_ = a?.GetCustomAttributes()
    ?.ToList()
    ?.Where(z => 1 == 0)
    ?.Where(z => 2 == 0)
    ?.Where(z => 3 == 0);
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task CheckArrayNullability() => """
int[]? array = null;
var z = array?.Single();
""".Verify(sourceType: SourceType.MethodBody);

#if NET6_0_OR_GREATER
    [Fact]
    public Task ConditionalToExtension() => """
[CreateSyncVersion]
public static async Task MethodAsync(IEnumerable<int>? integers)
{
    var res = integers?.Chunk(2).First();
}
""".Verify();
#endif
}
