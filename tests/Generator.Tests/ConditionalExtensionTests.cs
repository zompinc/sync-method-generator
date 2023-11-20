namespace Generator.Tests;

[UsesVerify]
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
}
