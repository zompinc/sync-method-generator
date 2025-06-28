namespace Generator.Tests;

public class TargetTypeLevelTests
{
    [Fact]
    public Task TargetInterface() => """
namespace Test;

[CreateSyncVersion]
public partial interface ITargetInterface
{
    Task MethodAsync();
}
""".Verify(sourceType: SourceType.Full);

    [Theory]
    [InlineData("class")]
    [InlineData("struct")]
    [InlineData("record")]
    [InlineData("record struct")]
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Globalization", "CA1307:Specify StringComparison for clarity", Justification = "Too many styling issues otherwise")]
    public Task TargetType(string type) => $$"""
namespace Test;

[CreateSyncVersion]
public partial {{type}} Target
{
    Task MethodAsync()
    {
        return Task.CompletedTask;
    }
}
""".Verify(
        disableUnique: true,
        sourceType: SourceType.Full,
        parameters: type.Replace(" ", string.Empty));

    [Fact]
    public Task SkipSyncVersion() => """
namespace Test;

[CreateSyncVersion]
public partial class SkipSyncVersion
{
    Task Method1Async()
    {
        return Task.CompletedTask;
    }

    [SkipSyncVersion]
    Task Method1Async()
    {
        return Task.CompletedTask;
    }
}
""".Verify(sourceType: SourceType.Full);
}
