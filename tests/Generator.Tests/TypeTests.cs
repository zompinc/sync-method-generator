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
    public Task NotPattern() =>
"_ = new object() is not DBNull;"
.Verify(false, true, sourceType: SourceType.MethodBody);

    [Fact]
    public Task VariableDeclarationRedundant() =>
"MemoryStream ms = new MemoryStream();"
.Verify(false, true, sourceType: SourceType.MethodBody);

    [Fact]
    public Task VariableDeclaration() =>
"MemoryStream ms = new();"
.Verify(false, true, sourceType: SourceType.MethodBody);

    [Fact]
    public Task BinaryPattern() =>
"_ = new object() is DBNull or Stream;"
.Verify(false, true, sourceType: SourceType.MethodBody);

    [Fact]
    public Task DeclarationExpression() =>
"new Dictionary<int, Stream>().TryGetValue(0, out Stream a);"
.Verify(false, true, sourceType: SourceType.MethodBody);

    [Fact]
    public Task NullableDeclarationExpression() =>
"new Dictionary<int, Stream>().TryGetValue(0, out Stream? a);"
.Verify(false, true, sourceType: SourceType.MethodBody);

    [Fact]
    public Task ConvertForeachType() => $$"""
foreach (Int32 i in new Int32[] { 1 })
{
}

await Task.CompletedTask;
""".Verify(false, true, sourceType: SourceType.MethodBody);

    [Fact]
    public Task NullableForeach()
    => """
foreach (Stream? i in Array.Empty<Stream?>())
{
}
"""
    .Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task NestedGenerics()
    => "var dict = new Dictionary<DateTime, List<int>>();"
        .Verify(sourceType: SourceType.MethodBody);

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
    public Task ArrayParameter() => """
[CreateSyncVersion]
public async Task MethodAsync(Int32[] o)
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
    public Task QualifiedGenericName()
        => "System.Collections.Generic.HashSet<byte> z = null!;"
        .Verify(sourceType: SourceType.MethodBody);

    [Theory]
    [InlineData("")]
    [InlineData("System.")]
    [InlineData("global::System.")]
    public Task QualifiedNonGenericName(string prefix)
        => $$"""
namespace System;

public partial class Class
{
    [CreateSyncVersion]
    public async Task MethodAsync()
    {
        {{prefix}}Security.Cryptography.CryptographicException z = null!;
    }
}
""".Verify(disableUnique: true, sourceType: SourceType.Full);

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
    public Task GenericClassWithGenericInnerClass() => """
namespace Test;

internal partial class Class<T>
{
    [CreateSyncVersion]
    public async Task FooAsync(Int<int> i) { }

    internal class Int<TU> { }
}
""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task EventHandlerType()
        => "_ = new DataReceivedEventHandler((s, e) => { });"
        .Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task CreateNullableType()
        => "new Dictionary<int, (int I, Stream? S)?>().TryGetValue(0, out (int I, Stream? S)? a);"
        .Verify(sourceType: SourceType.MethodBody);
}
