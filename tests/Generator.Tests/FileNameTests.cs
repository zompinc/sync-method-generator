namespace Generator.Tests;

public class FileNameTests
{
    [Fact]
    public Task DoNotCollideClassNames() => """
namespace Test;

public partial class Class
{
    [CreateSyncVersion]
    public async Task MethodAsync()
    {
    }
}

public partial class Class<T>
{
    [CreateSyncVersion]
    public async Task MethodAsync()
    {
    }
}

public partial class Class<T, T2>
{
    [CreateSyncVersion]
    public async Task MethodAsync()
    {
    }
}
""".Verify(sourceType: SourceType.Full);
}
