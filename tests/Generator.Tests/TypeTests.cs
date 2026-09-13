namespace Generator.Tests;

public class TypeTests
{
    [Fact]
    public Task ConvertExceptionType() => $$"""
try
{
    await Task.CompletedTask;
}
catch (OperationCanceledException)
{
}
""".Verify(false, true, sourceType: SourceType.MethodBody);

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public Task EnumPattern(bool isQualified) =>
$$"""
using static System.Data.ConnectionState;
namespace Test;

partial class Class
{
    [CreateSyncVersion]
    public Task MethodAsync()
    {
        _ = {{(isQualified ? "System.Data.ConnectionState." : string.Empty)}}Closed is System.Data.ConnectionState.Closed;
        return Task.CompletedTask;
    }
}
"""
.Verify(false, true, sourceType: SourceType.Full);

    [Fact]
    public Task EnumPatternName() =>
"""
enum Test { Test }

partial class Class
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public Task<bool> ReturnTrueAsync()
    {
        return Task.FromResult(Test.Test is Test.Test);
    }
}
"""
.Verify(false, true, sourceType: SourceType.Full);

    [Fact]
    public Task ConvertForeachType() => $$"""
foreach (Int32 i in new Int32[] { 1 })
{
}

await Task.CompletedTask;
""".Verify(false, true, sourceType: SourceType.MethodBody);

    [Fact]
    public Task TestTypes() => $$"""
String myStr;
string myStrPredefined;
Exception ex;
Int16 myShort;
Int16[] myShorts;
long myLong;

await Task.CompletedTask;
""".Verify(false, true, sourceType: SourceType.MethodBody);

    [Fact]
    public Task TwoDArrayParameter() => """
[CreateSyncVersion]
public async Task MethodAsync(Func<object[,], Task> o)
{
}
""".Verify();

    [Fact]
    public Task CastFullyQualifiedType() => """
class CustomClass { }

[CreateSyncVersion]
public async Task<object> GetCustomObjectAsync(object o)
{
    return (CustomClass)o;
}
""".Verify();

    [Fact]
    public Task CastFullyQualifiedTypeTwice() => """
class CustomClass { }

[CreateSyncVersion]
public async Task<object> GetCustomObjectAsync(object o)
{
    return (CustomClass)(object)(CustomClass)o;
}
""".Verify();

    [Fact]
    public Task UseFullyQualifiedTypeInIsExpression() => """
[CreateSyncVersion]
public async Task HasIsExpressionAsync(Stream stream) => _ = stream is FileStream;
""".Verify();

    [Fact]
    public Task HandleDiscardSymbol()
        => "_ = int.TryParse(\"2\", out _);"
        .Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task HandleTypeOf()
        => "_ = typeof(Stream);"
        .Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task HandleTuple()
        => "[CreateSyncVersion]public async Task MethodAsync((Stream S, int I) z) { }"
        .Verify();

    [Fact]
    public Task HandleNullableTuple()
        => "[CreateSyncVersion]public async Task MethodAsync((Stream? S, int I) z) { }"
        .Verify();

    [Fact]
    public Task HandleNameOf()
        => "_ = nameof(Stream);"
        .Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task HandleAsCast()
        => "_ = new object() as Stream;"
        .Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task HandleNameOfGenericTuple()
        => "_ = nameof(IEnumerable<(Stream? S, int I)>);"
        .Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task SwitchType() => """
[CreateSyncVersion]
public async Task SwitchAsync(Stream stream)
{
    var s = stream switch
    {
        FileStream fs => fs,
        _ => throw new InvalidOperationException("No"),
    };
}
""".Verify();

    [Fact]
    public Task EventHandlerType()
        => "_ = new DataReceivedEventHandler((s, e) => { });"
        .Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task CreateNullableType()
        => "new Dictionary<int, (int I, Stream? S)?>().TryGetValue(0, out (int I, Stream? S)? a);"
        .Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task SemaphoreSlimWaitAndRelease()
        => """
private SemaphoreSlim semaphore = new(1, 1);

[CreateSyncVersion]
public async Task MethodAsync(CancellationToken ct = default) 
{
    await semaphore.WaitAsync(ct);

    try
    {
        await Task.Delay(100, ct);
    }
    finally
    {
        semaphore.Release();
    }
}
""".Verify();
}
