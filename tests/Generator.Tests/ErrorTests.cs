namespace Generator.Tests;

[UsesVerify]
public class ErrorTests
{
    [Fact]
    public Task NotPartialClassStillGenerates()
    {
        var source = """
namespace Test;
public static class NotPartialClass
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    async void EmptyAsync()
    {
    }
}
""";
        return TestHelper.Verify(source);
    }
}
