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

    [Fact]
    public Task ShortenLongFileName() => """
namespace A.Long.Enough.Namespace.To.Make.The.Generated.File.Name.Exceed.What.Is.Reasonable;

public partial class OuterClass
{
    public partial class InnerClass
    {
        [CreateSyncVersion]
        public async Task MethodAsync()
        {
        }
    }
}
""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task ShortenLongFileNameWhenTheMethodNameFillsIt() => """
namespace A.Long.Enough.Namespace.To.Make.The.Generated.File.Name.Exceed.What.Is.Reasonable;

public partial class OuterClass
{
    [CreateSyncVersion]
    public async Task ThisMethodHasAnUnreasonablyLongNameWhichOnItsOwnLeavesNoRoomForTheNamespaceOrTheContainingTypeAsync()
    {
    }
}
""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task ShortenLongFileNameKeepsOverloadsApart() => """
namespace A.Long.Enough.Namespace.To.Make.The.Generated.File.Name.Exceed.What.Is.Reasonable;

public partial class OuterClass
{
    public partial class InnerClass
    {
        [CreateSyncVersion]
        public async Task MethodAsync()
        {
        }

        [CreateSyncVersion]
        public async Task MethodAsync(int i)
        {
        }
    }
}
""".Verify(sourceType: SourceType.Full);
}
