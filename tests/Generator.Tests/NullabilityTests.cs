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
    public Task NullableOmitExplicit() => """
#nullable disable

namespace Test;

partial class MyClass
{
    [Obsolete]
    [CreateSyncVersion(OmitNullableDirective = true)]
    public async Task MethodAsync()
    {
        string f = null;
    }
}
""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task ConditionalNullable() => """
#nullable disable

namespace Test;

partial class MyClass
{
    [CreateSyncVersion(OmitNullableDirective = true)]
    public async Task MethodAsync(Stream stream)
    {
        _ = stream?.DoSomething();
    }
}

internal static class Extension
{
    public static Stream DoSomething(this Stream stream) => stream;
}
""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task ConditionalNullableTwice() => """
#nullable disable

namespace Test;

partial class MyClass
{
    [CreateSyncVersion(OmitNullableDirective = true)]
    public async Task MethodAsync(Stream stream)
    {
        _ = stream?.DoSomething()?.DoSomething();
    }
}

internal static class Extension
{
    public static Stream DoSomething(this Stream stream) => stream;
}
""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task ConditionalNullableStructTwice() => """
#nullable disable

namespace Test;

partial class MyClass
{
    [CreateSyncVersion(OmitNullableDirective = true)]
    public async Task MethodAsync(int? myInt)
    {
        _ = myInt?.DoSomething().DoSomething();
    }
}

internal static class Extension
{
    public static int DoSomething(this int myInt) => myInt;
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
