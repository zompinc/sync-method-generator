namespace Generator.Tests;

public class DelegateTests
{
    [Theory]
    [InlineData("await Task.CompletedTask;")]
    [InlineData("{ await Task.CompletedTask; };")]
    public Task DropCompletedTaskFromLambda(string statements)
        => $"var delAsync = async () => {statements}"
        .Verify(disableUnique: true, sourceType: SourceType.MethodBody);

    [Fact]
    public Task AsyncDelegate() => """
var delAsync = async () => await Task.CompletedTask;
await delAsync();
""".Verify(sourceType: SourceType.MethodBody);

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public Task AsyncDelegateWithCancellationToken(bool explicitParameter)
    {
        var @explicit = explicitParameter ? "CancellationToken " : string.Empty;
        return $"""
Func<CancellationToken, Task<int>> delAsync = async ({@explicit}ct) => await Task.FromResult(2);
var result = await delAsync(CancellationToken.None);
""".Verify(disableUnique: true, sourceType: SourceType.MethodBody);
    }

    [Fact]
    public Task AsyncDelegateWithParameter()
        => "Func<Point, Size, Task> delAsync = async (p, s) => await Task.CompletedTask;"
        .Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task AsyncVarDelegateWithParameter()
        => "var delAsync = async (Point p, Size s) => await Task.CompletedTask;"
        .Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task AsyncVarDelegateWithParameterAndReturnType()
        => "var delAsync = async (Point p, Size s) => await Task.FromResult(p);"
        .Verify(sourceType: SourceType.MethodBody);

    [Theory]
    [InlineData("null!")]
    [InlineData("new Progress<int>()")]
    public Task AsyncDelegateWithIProgress(string iProgressArg) => $"""
Func<IProgress<int>, CancellationToken, Task<int>> delAsync = async (p, ct) => await Task.FromResult(2);
var result = await delAsync({iProgressArg}, CancellationToken.None);
""".Verify(disableUnique: true, sourceType: SourceType.MethodBody);

    [Theory]

    [InlineData("null")]
    [InlineData("new Progress<int>()")]
    public Task AsyncDelegateWithMullableIProgress(string iProgressArg) => $"""
Func<IProgress<int>?, CancellationToken, Task<int>> delAsync = async (p, ct) => await Task.FromResult(2);
var result = await delAsync({iProgressArg}, CancellationToken.None);
""".Verify(disableUnique: true, sourceType: SourceType.MethodBody);

    [Fact]
    public Task AsyncDelegateExplicit() => """
Func<System.Threading.Tasks.Task> delAsync = async () => await Task.CompletedTask;
await delAsync();
""".Verify(sourceType: SourceType.MethodBody);
}
