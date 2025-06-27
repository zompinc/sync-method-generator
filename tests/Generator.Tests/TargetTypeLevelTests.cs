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

    [Fact]
    public Task TargetClass() => """
namespace Test;

[CreateSyncVersion]
public partial class TargetClass
{
    Task MethodAsync()
    {
        return Task.CompletedTask;
    }
}
""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task TargetStruct() => """
namespace Test;

[CreateSyncVersion]
public partial struct TargetStruct
{
    Task MethodAsync()
    {
        return Task.CompletedTask;
    }
}
""".Verify(sourceType: SourceType.Full);

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
