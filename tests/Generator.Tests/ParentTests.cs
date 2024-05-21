namespace Generator.Tests;

public class ParentTests
{
    [Fact]
    public Task Struct() => """
namespace Test;
public partial struct Struct
{
    [CreateSyncVersion]
    public readonly async Task MethodAsync() => await Task.Delay(1000);
}
""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task Record() => """
namespace Test;
public partial record Record
{
    [CreateSyncVersion]
    public async Task MethodAsync() => await Task.Delay(1000);
}
""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task RecordStruct() => """
namespace Test;
public partial record struct RecordStruct
{
    [CreateSyncVersion]
    public readonly async Task MethodAsync() => await Task.Delay(1000);
}
""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task RecordClass() => """
namespace Test;
public partial record class Record
{
    [CreateSyncVersion]
    public async Task MethodAsync() => await Task.Delay(1000);
}
""".Verify(sourceType: SourceType.Full);
}
