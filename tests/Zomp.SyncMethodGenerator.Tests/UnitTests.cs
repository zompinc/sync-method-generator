namespace Zomp.SyncMethodGenerator.Tests;

[UsesVerify]
public class UnitTests
{
    [Fact]
    public Task MultipleNamespaces()
    {
        // The source code to test
        var source = """
namespace NsOne
{
    namespace NsTwo.NsThree
    {
        namespace NsFour
        {
            public partial class MultipleNamespaces
            {
                [Zomp.SyncMethodGenerator.CreateSyncVersion]
                async void EmptyAsync()
                {
                }
            }
        }
    }
}
""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task MultipleClasses()
    {
        // The source code to test
        var source = """
namespace NsOne
{
    public partial class C1
    {
        internal partial class C2
        {
            private protected partial class C3
            {
                private partial class C4
                {
                    [Zomp.SyncMethodGenerator.CreateSyncVersion]
                    async void EmptyAsync()
                    {
                    }
                }
            }
        }
    }
}

""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task MemoryToSpan()
    {
        // The source code to test
        var source = """
using System;
using System.IO;
using System.Threading;
using System.Threading.Tasks;

namespace Test;

public partial class Stuff
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    private async Task ReadAsMemoryAsync(Stream stream, byte[] sampleBytes, CancellationToken ct = default)
        => await stream.ReadAsync(sampleBytes.AsMemory(0, 123), ct).ConfigureAwait(false);
}
""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task MemoryToSpanWithBody()
    {
        // The source code to test
        var source = """
using System;
using System.IO;
using System.Threading;
using System.Threading.Tasks;

namespace Test;

public partial class Stuff
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    private async Task ReadAsMemoryAsync(Stream stream, byte[] sampleBytes, CancellationToken ct = default)
    {
        await stream.ReadAsync(sampleBytes.AsMemory(0, 123), ct).ConfigureAwait(false);
    }
}
""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task ReadOnlyMemoryToReadOnlySpan()
    {
        // The source code to test
        var source = """
using System;
using System.IO;
using System.Threading;
using System.Threading.Tasks;

namespace Test;

public partial class Stuff
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    static async Task WriteAsync(ReadOnlyMemory<byte> buffer, Stream stream, CancellationToken ct)
        => await stream.WriteAsync(buffer, ct).ConfigureAwait(true);
}
""";
        return TestHelper.Verify(source);
    }


    [Fact]
    public Task NonPredefinedTypes()
    {
        var source = """
using N2;
using static N2.C1;
using System;
using System.Threading.Tasks;

namespace N1
{
    partial class Stuff
    {
        [Zomp.SyncMethodGenerator.CreateSyncVersion]
        async Task FillAsync(Memory<C2.Accelerometer> accelerometers)
        {
            C2.Accelerometer a = new(1, 2, 3);
            accelerometers.Span[0] = a;
        }
    }
}

namespace N2
{
    internal class C1
    {
        public class C2
        {
            public record struct Accelerometer(short X, short Y, short Z);
        }
    }
}
""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task TaskOfT()
    {
        var source = """
using System.Threading.Tasks;
using System.Drawing;

namespace Test;
partial class Stuff
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public static async Task<Point> GetPointAsync()
    {
        return await Task.FromResult(new Point(1, 2));
    }
}
""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task TaskOfTArray()
    {
        var source = """
using System.Threading.Tasks;
using System.Drawing;

namespace Test;
partial class Stuff
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public static async Task<Point[]> GetPointAsync()
    {
        return await Task.FromResult(new[] { new Point(1, 2) });
    }
}
""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task DropBrackets()
    {
        var source = """
using System.Threading.Tasks;
using System.Drawing;

namespace Test;
partial class Stuff
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public static async Task<int> GetIntAsync()
    {
        return (await Task.FromResult(new[] { 1, 2 }))[0];
    }
}
""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task TaskOfTIList()
    {
        var source = """
using System.Collections.Generic;
using System.Drawing;
using System.Threading.Tasks;

namespace Test;
partial class Stuff
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public static async Task<IList<Point>> GetPointAsync()
    {
        return await Task.FromResult(new[] { new Point(1, 2) });
    }
}
""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task TaskOfT2IList()
    {
        var source = """
using System.Collections.Generic;
using System.Drawing;
using System.Threading.Tasks;

namespace Test;
partial class Stuff
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public static async Task<IList<T>> GetArrayOfTAsync<T>() where T: new()
    {
        return await Task.FromResult(new T[] { new T() });
    }
}
""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task DefaultParameter()
    {
        var source = """
using System.IO;
using System.Threading.Tasks;

namespace Test;
partial class Stuff
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    [CreateSyncVersion]
    public async Task<int> GetColorAsync(FileAccess access = FileAccess.Read)
        => await Task.FromResult(1);
}
""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task TaskOfArayOfGeneric()
    {
        var source = """
using System.Collections.Generic;
using System.Drawing;
using System.Threading.Tasks;

namespace Test;
partial class Stuff
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public static async Task<LinkedListNode<Point>[]> GetArrayOfTAsync<T>() where T : new()
    {
        return await Task.FromResult(new LinkedListNode<Point>[] { });
    }
}
""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task WithAction()
    {
        var source = """
using System;
using System.Drawing;
using System.Threading.Tasks;
using System.Collections.Generic;

namespace Test;
partial class Stuff
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public static async Task WithAction(Action<Point, IEnumerable<Point>>? action) { }
}
""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task LocalFunction()
    {
        var source = """
using System.IO;
using System.Threading;
using System.Threading.Tasks;

namespace Test;
partial class Stuff
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public static async Task<int> InternalExampleAsync(Stream stream, CancellationToken ct)
    {
        static async Task<int> Internal(Stream stream, CancellationToken ct)
        {
            var buf = new byte[1];
            return await stream.ReadAsync(buf, 0, 1, ct);
        }
        return await Internal(stream, ct);
    }
}
""";
        return TestHelper.Verify(source);
    }
}
