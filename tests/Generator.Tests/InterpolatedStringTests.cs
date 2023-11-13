namespace Generator.Tests;

[UsesVerify]
public class InterpolatedStringTests
{
    [Theory]
    [InlineData("await File.ReadAllTextAsync(\"123\")")]
    [InlineData("(await File.ReadAllTextAsync(\"123\"))")]
    public Task EnsureParentheses(string call) => $$"""
var z = $"123{{{call}}}456";
""".Verify(false, true, sourceType: SourceType.MethodBody);

    [Theory]
    [InlineData("await File.ReadAllTextAsync(\"123\")")]
    [InlineData("(await File.ReadAllTextAsync(\"123\"))")]
    public Task HandleFormatString(string call) => $$"""
var z = $"123{{{call}}:hh}456";
""".Verify(false, true, sourceType: SourceType.MethodBody);
}
