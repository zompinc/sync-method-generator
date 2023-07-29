namespace Generator.Tests;

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
    public Task GenericClass()
    {
        // The source code to test
        var source = """
namespace Test;
partial class GenericClass<T1, T2> where T1 : struct where T2 : class
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    async void EmptyAsync()
    {
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

#if NETCOREAPP1_0_OR_GREATER
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

    [InlineData(false)]
    [InlineData(true)]
    [Theory]
    public Task MemorySpanProperty(bool explicitType)
    {
        var typeName = explicitType ? "Memory<byte>" : "var";
        // The source code to test
        var source = $$"""
using System;
using System.IO;
using System.Threading;
using System.Threading.Tasks;

namespace Test;

public partial class Stuff
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    private async Task MakeArray(byte[] sampleBytes, CancellationToken ct = default)
    {
        {{typeName}} mem = sampleBytes.AsMemory(0, 123);
        var arr = mem.Span.ToArray();
    }
}
""";
        return TestHelper.Verify(source, false, false, explicitType);
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
#endif


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
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
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

    [Fact]
    public Task LocalFunctionDelegateWithEndingAsync()
    {
        var source = """
using System;
using System.Threading;
using System.Threading.Tasks;

namespace Test;
partial class Stuff
{
    static async Task<int> SomeMethodAsync(Func<CancellationToken, Task<int>> _)
        => await Task.FromResult(0);

    static int SomeMethod(Func<int> _) => 0;

    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public static async Task DoItAsync()
    {
        static async Task<int> LocalAsync(CancellationToken ct)
        {
            return await Task.FromResult(0);
        }

        _ = await SomeMethodAsync(LocalAsync);
    }
}
""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task UnwrapExtensionMethod()
    {
        var source = """
using System.Threading;
using System.Threading.Tasks;

namespace Zomp.SyncMethodGenerator.IntegrationTests
{
    using Extensi.ons123;
    internal partial class ExtensionMethods
    {
        [Zomp.SyncMethodGenerator.CreateSyncVersion]
        public static async Task ZeroParamsAsync(object o, CancellationToken ct) => await o.SomeMethodAsync(ct);
        [Zomp.SyncMethodGenerator.CreateSyncVersion]
        public static async Task OneParamsAsync(object o, string s, CancellationToken ct) => await o.SomeMethodAsync(s, ct);
        [Zomp.SyncMethodGenerator.CreateSyncVersion]
        public static async Task TwoParamsAsync(object o, string s, int i, CancellationToken ct) => await o.SomeMethodAsync(s, i, ct);
    }
}

namespace Extensi.ons123
{
    internal static class MyExtensionClass
    {
        public static async Task SomeMethodAsync(this object _, CancellationToken _1) => await Task.CompletedTask;
        public static void SomeMethod(this object _) { }
        public static async Task SomeMethodAsync(this object _, string _2, CancellationToken _1) => await Task.CompletedTask;
        public static void SomeMethod(this object _, string _2) { }
        public static async Task SomeMethodAsync(this object _, string _2, int _3, CancellationToken _1) => await Task.CompletedTask;
        public static void SomeMethod(this object _, string _2, int _3) { }
    }
}
""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task Preprocessor()
    {
        var source = """
using System.Threading;
using System.Threading.Tasks;

namespace Test;

public partial class Stuff
{
#if false
These comments shouldn't show
#endif
#if true
#else
#endif
#region R1
#if true
    /// <summary>
    /// A summary
    /// </summary>
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public async Task WrappedAsync() => await Task.CompletedTask;
#endif
#endregion
}

""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task UnwrapGenericExtensionMethod()
    {
        var source = """
using System.Drawing;
using System.Threading;
using System.Threading.Tasks;

namespace Zomp.SyncMethodGenerator.IntegrationTests
{
    using Extensi.ons123;
    internal partial class Extensions
    {
        [Zomp.SyncMethodGenerator.CreateSyncVersion]
        public static async Task HasGenericExtensionAsync(object o, CancellationToken ct)
        {
            var z = o.TryGetValue<Point>(out var item);
        }

        [Zomp.SyncMethodGenerator.CreateSyncVersion]
        public static async Task HasGeneric2ExtensionAsync(object o, CancellationToken ct)
        {
            var z = o.TryGetValue<Point, PointF>(out var _, out var _1);
        }
    }
}

namespace Extensi.ons123
{
    internal static class MyExtensionClass
    {
        public static bool TryGetValue<T>(this object _, out T? item)
        {
            item = default;
            return false;
        }

        public static bool TryGetValue<T1, T2>(this object _, out T1? item1, out T2? item2)
        {
            item1 = default;
            item2 = default;
            return false;
        }
    }
}
""";
        return TestHelper.Verify(source);
    }

#if NETCOREAPP1_0_OR_GREATER
    [Fact]
    public Task Overloads()
    {
        var source = """
using System;
using System.IO;
using System.Threading;
using System.Threading.Tasks;

namespace Zomp.SyncMethodGenerator.IntegrationTests;

internal partial class OverloadsNS
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    async Task ReadAsMemoryAsync(Stream stream, byte[] sampleBytes, CancellationToken ct)
        => await stream.ReadAsync(sampleBytes.AsMemory(0, 123), ct).ConfigureAwait(false);

    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    async Task ReadAsMemoryAsync(Stream stream, byte[] sampleBytes)
        => await stream.ReadAsync(sampleBytes.AsMemory(0, 123)).ConfigureAwait(false);
}
""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task FindsASpan()
    {
        var source = """
using System;
using System.IO;
using System.Threading;
using System.Threading.Tasks;

namespace Zomp.SyncMethodGenerator.IntegrationTests;

internal partial class Stuff
{
    class ClassThatReturnsMemoryAndSpan
    {
        public Memory<byte> GetMemory(int sizeHint = 0) => throw new NotImplementedException();
        public Span<byte> GetSpan(int sizeHint = 0) => throw new NotImplementedException();
    }

    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    private async Task GetMemoryOrSpanAsync()
    {
        var instance = new ClassThatReturnsMemoryAndSpan();
        Memory<byte> mem = instance.GetMemory();
        var arr = mem.Span.ToArray();
    }
}
""";
        return TestHelper.Verify(source);
    }
#endif

    [Theory]
    [InlineData("progress")]
    [InlineData("progress as IProgress<float>")]
    [InlineData("ProgressFunc(progress)")]
    [InlineData("(Progress<float>)progress")]
    [InlineData("(progress)")]
    [InlineData("someBool ? progress : null")]
    [InlineData("someBool ? null : progress")]
    [InlineData("classWithProgress.Property")]
    [InlineData("array[0]")]
    [InlineData("customProgress++")]
    [InlineData("customProgress + customProgress")]
    [InlineData("(Progress<float>)classWithProgress")]
    [InlineData("new CustomProgress()")]
    [InlineData("SomeMethod(progress)")]
    public Task DropIProgressExpressionArgument(string callArgument)
    {
        var source = $$"""
using System;
using System.Threading;
using System.Threading.Tasks;

namespace Zomp.SyncMethodGenerator.IntegrationTests;

internal partial class Stuff
{
    public static async Task WithIProgress()
    {
    }

    public static async Task WithIProgressAsync(IProgress<float>? progress = null)
    {
    }

    static Func<IProgress<float>?, IProgress<float>?> ProgressFunc = (p) => p;

    bool someBool = true;

    class ClassWithProgress
    {
        Progress<float> pg = new();
        public Progress<float> Property => pg;
        public static implicit operator Progress<float>(ClassWithProgress a) => a.pg;
    }

    class CustomProgress : IProgress<float>
    {
        public static CustomProgress operator ++(CustomProgress a) => a;
        public static CustomProgress operator +(CustomProgress a, CustomProgress b) => a;
        public void Report(float value) => throw new NotImplementedException();
    }

    static CustomProgress customProgress = new();

    static Progress<float>[] array = Array.Empty<Progress<float>>();

    static ClassWithProgress classWithProgress = new();

    static IProgress<T> SomeMethod<T>(IProgress<T> p) => p;

    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public static async Task CallWithIProgressAsync()
    {
        var progress = new Progress<float>();
        await WithIProgressAsync({{callArgument}});
    }
}
""";
        return TestHelper.Verify(source, false, true);
    }
}
