namespace Generator.Tests;

public class NullabilityTests
{
    [Fact(Skip = "Execute when resolved: https://github.com/dotnet/roslyn/issues/49555")]
    public Task NullableDisable() => """
#nullable disable

namespace Test;

internal partial class WithNullableDisabled
{
    [CreateSyncVersion]
    public async Task DoNothingAsync(string l)
    {
        l = null;
        await Task.CompletedTask;
    }
}
""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task NoNullableSupport() => """
namespace Test;

partial class WithNullableDisabled
{
    [CreateSyncVersion]
    public async Task MethodAsync(string l) => await Task.CompletedTask;
}
""".Verify(sourceType: SourceType.Full, languageVersion: LanguageVersion.CSharp7_3);
}
