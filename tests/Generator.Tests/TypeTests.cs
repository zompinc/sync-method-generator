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
long myLong;

await Task.CompletedTask;
""".Verify(false, true, sourceType: SourceType.MethodBody);

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
    public Task HandleNameOf()
        => "_ = nameof(Stream);"
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
}
