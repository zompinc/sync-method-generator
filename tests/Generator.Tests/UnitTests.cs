namespace Generator.Tests;

public class UnitTests
{
    [Fact]
    public Task MultipleNamespaces() => """
namespace NsOne
{
    namespace NsTwo.NsThree
    {
        namespace NsFour
        {
            public partial class MultipleNamespaces
            {
                [CreateSyncVersion]
                async void EmptyAsync()
                {
                }
            }
        }
    }
}
""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task StaticUsings() => """
using static N2.C2;

namespace N1
{
    public partial class C1
    {
        [CreateSyncVersion]
        public async Task MethodAsync()
        {
            _ = OtherConst;
        }
    }
}

namespace N2
{
    public class C2
    {
        public const int OtherConst = 1;
    }
}

""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task GenericClass() => """
namespace Test;
partial class GenericClass<T1, T2> where T1 : struct where T2 : class
{
    [CreateSyncVersion]
    async void EmptyAsync()
    {
    }
}
""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task GenericClassMemberAccess() => """
namespace Test;
partial class GenericClass<T>
{
    private const string Bar = "Bar";

    [CreateSyncVersion]
    public async Task MethodAsync()
    {
        _ = Bar;
    }
}

""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task GenericAsyncCall() => """
namespace Test;
partial class GenericClass<T>
{
    [CreateSyncVersion]
    public async Task<T> FooAsync<T>(CancellationToken ct = default)
        => await this.InnerFooAsync<T>(ct);

    private async Task<T> InnerFooAsync<T>(CancellationToken ct = default) => default;
    private async T InnerFoo<T>() => default;
}
""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task DropCompletedTask() => """
[CreateSyncVersion]
public async Task ExecAsync(CancellationToken ct)
{
    await Task.CompletedTask;
}
""".Verify();

    [Theory]
    [InlineData("{ return Task.CompletedTask; }")]
    [InlineData("{ return Task.CompletedTask; Console.WriteLine(\"123\"); }")]
    [InlineData("=> Task.CompletedTask;")]
    public Task DropUnawaitedCompletedTask(string statement) => $"""
[Zomp.SyncMethodGenerator.CreateSyncVersion]
public static Task DoSomethingAsync() {statement}
""".Verify(disableUnique: true);

    [Theory]
    [InlineData("{ return default; }")]
    [InlineData("{ return new(); }")]
    [InlineData("{ return new ValueTask(); }")]
#if NETCOREAPP1_0_OR_GREATER
    [InlineData("{ return ValueTask.CompletedTask; }")]
    [InlineData("{ return ValueTask.CompletedTask; Console.WriteLine(\"123\"); }")]
    [InlineData("=> ValueTask.CompletedTask;")]
#endif
    public Task DropUnawaitedCompletedValueTask(string statement) => $"""
[Zomp.SyncMethodGenerator.CreateSyncVersion]
public static ValueTask DoSomethingAsync() {statement}
""".Verify(disableUnique: true);

    [Fact]
    public Task KeepDefaultValueTaskWithResult() => $"""
[Zomp.SyncMethodGenerator.CreateSyncVersion]
public static ValueTask<int> ReturnDefault() => default;
""".Verify();

    [Theory]
    [InlineData("{ return new(1); }")]
    [InlineData("{ return new ValueTask<int>(1); }")]
#if NETCOREAPP1_0_OR_GREATER
    [InlineData("{ return ValueTask.FromResult(1); }")]
#endif
    public Task ReturnValueTaskInstance(string statement) => $"""
[Zomp.SyncMethodGenerator.CreateSyncVersion]
public static ValueTask<int> ReturnAsync() {statement}
""".Verify(disableUnique: true);

    [Fact]
    public Task ReturnValueTask() => """
[CreateSyncVersion]
private ValueTask GetMemoryOrSpanAsync(bool input)
{
    if (input)
    {
        return ReturnAsync();
    }
    return ReturnAsync();
}

private ValueTask ReturnAsync() => default;
private void Return() { }
""".Verify();

    [Fact]
    public Task ReturnValueTaskNoBody() => """
[CreateSyncVersion]
private ValueTask ReturnAsync(bool input)
{
    if (input) return ReturnAsync();
    return ReturnAsync();
}

private ValueTask ReturnAsync() => default;
private void Return() { }
""".Verify();

    [Fact]
    public Task ReturnTask() => """
[CreateSyncVersion]
private Task ReturnAsync(bool input)
{
    if (input)
    {
        return ReturnAsync();
    }
    return ReturnAsync();
}

private Task ReturnAsync() => default;
private void Return() { }
""".Verify();

    [Fact]
    public Task ReturnTaskNoBody() => """
[CreateSyncVersion]
private Task ReturnAsync(bool input)
{
    if (input) return ReturnAsync();
    return ReturnAsync();
}

private Task ReturnAsync() => default;
private void Return() { }
""".Verify();

    [Fact]
    public Task MultipleClasses() => """
namespace NsOne
{
    public partial class C1
    {
        partial class C2
        {
            private protected partial class C3
            {
                private partial class C4
                {
                    [CreateSyncVersion]
                    async void EmptyAsync()
                    {
                    }
                }
            }
        }
    }
}

""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task HandleAttribute() => """
using System.Diagnostics.CodeAnalysis;

namespace Test;

public partial class Class
{
    [CreateSyncVersion]
    public async Task<int> ReadSomeBytesAsync(Stream stream)
    {
        var buffer = new byte[100];
        return await stream.ReadAsync(buffer, 0, 100);
    }
}
""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task HandleAttributeOnLocalFunction() => """
[Zomp.SyncMethodGenerator.CreateSyncVersion]
void Local()
{
}

Local();
await Task.CompletedTask;
""".Verify(sourceType: SourceType.MethodBody);

#if NETCOREAPP1_0_OR_GREATER
    [Fact]
    public Task DoNotDropInvocationChainThatHasAsyncAnywhere() => """
[CreateSyncVersion]
public static async Task WriteAllTextAsync(string file, string contents,
CancellationToken ct)
{
    await File.WriteAllTextAsync(file, contents, ct).ConfigureAwait(true);
}
""".Verify();
#endif

    [Fact]
    public Task CallWithTypeParameters() => """
[CreateSyncVersion]
public async Task MyFuncAsync<T>()
{
    await MyFuncAsync<T>();
}
""".Verify();

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public Task TaskOfT(bool explicitType) => $$"""
[CreateSyncVersion]
public static async Task<Point> GetPointAsync()
{
    return await Task.FromResult{{(explicitType ? "<Point>" : string.Empty)}}(new Point(1, 2));
}
""".Verify(disableUnique: true);

    [Fact]
    public Task TaskOfTArray() => """
[CreateSyncVersion]
public static async Task<Point[]> GetPointAsync()
{
    return await Task.FromResult(new[] { new Point(1, 2) });
}
""".Verify();

    [Fact]
    public Task DropBrackets() => """
[CreateSyncVersion]
public static async Task<int> GetIntAsync()
{
    return (await Task.FromResult(new[] { 1, 2 }))[0];
}
""".Verify();

    [Fact]
    public Task TaskOfTIList() => """
[CreateSyncVersion]
public static async Task<IList<Point>> GetPointAsync()
{
    return await Task.FromResult(new[] { new Point(1, 2) });
}
""".Verify();

    [Fact]
    public Task TaskOfT2IList() => """
[CreateSyncVersion]
public static async Task<IList<T>> GetArrayOfTAsync<T>() where T: new()
{
    return await Task.FromResult(new T[] { new T() });
}
""".Verify();

    [Fact]
    public Task DefaultParameter() => """
[CreateSyncVersion]
public async Task GetColorAsync(FileAccess access = FileAccess.Read) { }
""".Verify();

    [Fact]
    public Task TaskOfArrayOfGeneric() => """
[CreateSyncVersion]
public static async Task<LinkedListNode<Point>[]> GetArrayOfTAsync<T>() where T : new()
{
    return await Task.FromResult(new LinkedListNode<Point>[] { });
}
""".Verify();

    [Theory]
    [InlineData("await w.Delay(ct);")]
    [InlineData("await w.AsyncCondition(ct);")]
    [InlineData("if (await w.AsyncCondition(ct)){}")]
    [InlineData("if (!await w.AsyncCondition(ct)){}")]
    public Task DoNotCallPropertiesReturningGenericTasks(string invocation) => $$"""
private readonly ClassWithAsyncFunc w = new();

[CreateSyncVersion]
public async Task MyMethodAsync(CancellationToken ct)
{
    {{invocation}}
}

private class ClassWithAsyncFunc
{
    internal Func<CancellationToken, Task<bool>> AsyncCondition
        => (ct) => Task.FromResult(false);
    internal Func<CancellationToken, Task<bool>> Delay
        => (ct) => Task.FromResult(false);
}
""".Verify(false, true);

    [Fact]
    public Task DoNotCallPropertiesReturningGenericTasksExpression() => $$"""
private readonly ClassWithAsyncFunc w = new();

[CreateSyncVersion]
public async Task MyMethodAsync(CancellationToken ct)
    => await w.AsyncCondition(ct);

private class ClassWithAsyncFunc
{
    internal Func<CancellationToken, Task<bool>> AsyncCondition
        => (ct) => Task.FromResult(false);
}
""".Verify(false, true);

    [Theory]
    [InlineData("AsyncAction")]
    [InlineData("Delay")]
    public Task DoNotCallPropertiesReturningTasks(string methodName) => $$"""
private readonly ClassWithAsyncFunc w = new();

[CreateSyncVersion]
public async Task MyMethodAsync()
{
    await w.{{methodName}}(CancellationToken.None);
}

private class ClassWithAsyncFunc
{
    internal Func<CancellationToken, Task> {{methodName}}
        => (ct) => Task.CompletedTask;
}
""".Verify(disableUnique: true);

    [Fact]
    public Task CallPropertiesReturningTasksWithAsync() => """
private readonly ClassWithAsyncFunc w = new();

[CreateSyncVersion]
public async Task MyMethodAsync()
{
    await w.ActionAsync(CancellationToken.None);
}

private class ClassWithAsyncFunc
{
    internal Func<CancellationToken, Task> ActionAsync
        => (ct) => Task.CompletedTask;

    internal void Action()
    {
    }
}
""".Verify();

    [Fact]
    public Task WithAction() => """
[CreateSyncVersion]
public static async Task WithAction(Action<Point, IEnumerable<Point>>? action) { }
""".Verify();

    [Fact]
    public Task MultipleInitializers() => """
[CreateSyncVersion]
public static async Task MethodAsync(CancellationToken ct)
{
    int a = await PrivateClass.FromResult(2, 6), b = await Task.FromResult(2),  c = await pc.IntProperty(ct);
}

PrivateClass pc = new PrivateClass();
private class PrivateClass
{
    internal Func<CancellationToken, Task<int>> IntProperty => (ct) => Task.FromResult(2);

    public static Task<TResult> FromResult<TResult>(TResult delay, int unrelated) => Task.FromResult(delay);
}
""".Verify();

    [Fact]
    public Task LocalFunction() => """
[CreateSyncVersion]
public static async Task<int> InternalExampleAsync(Stream stream, CancellationToken ct)
{
    static async Task<int> Internal(Stream stream, CancellationToken ct)
    {
        var buf = new byte[1];
        return await stream.ReadAsync(buf, 0, 1, ct);
    }
    return await Internal(stream, ct);
}
""".Verify();

    [Fact]
    public Task LocalFunctionNonGeneric() => """
[CreateSyncVersion]
public static async Task<int> InternalExampleAsync(Stream stream, CancellationToken ct)
{
    static async Task LocalFuncAsync()
    {
        await Task.CompletedTask;
    }

    await LocalFuncAsync();
}
""".Verify();

    [Fact]
    public Task LocalFunctionDelegateWithEndingAsync() => """
static async Task<int> SomeMethodAsync(Func<CancellationToken, Task<int>> _)
    => await Task.FromResult(0);

static int SomeMethod(Func<int> _) => 0;

[CreateSyncVersion]
public static async Task DoItAsync()
{
    static async Task<int> LocalAsync(CancellationToken ct)
    {
        return await Task.FromResult(0);
    }

    _ = await SomeMethodAsync(LocalAsync);
}
""".Verify();

    [Fact]
    public Task UnwrapExtensionMethod() => """
using System.Threading;
using System.Threading.Tasks;

namespace Zomp.SyncMethodGenerator.IntegrationTests
{
    using Extensi.ons123;
    partial class ExtensionMethods
    {
        [CreateSyncVersion]
        public static async Task ZeroParamsAsync(object o, CancellationToken ct) => await o.SomeMethodAsync(ct);
        [CreateSyncVersion]
        public static async Task OneParamsAsync(object o, string s, CancellationToken ct) => await o.SomeMethodAsync(s, ct);
        [CreateSyncVersion]
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
""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task Preprocessor() => """
using System.Threading;
using System.Threading.Tasks;

namespace Test;

public partial class Class
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
    [CreateSyncVersion]
    public async Task WrappedAsync() => await Task.CompletedTask;
#endif
#endregion
}

""".Verify(sourceType: SourceType.Full);

    [Fact]
    public Task UnwrapGenericExtensionMethod() => """
using System.Drawing;
using System.Threading;
using System.Threading.Tasks;

namespace Zomp.SyncMethodGenerator.IntegrationTests
{
    using Extensi.ons123;
    partial class Extensions
    {
        [CreateSyncVersion]
        public static async Task HasGenericExtensionAsync(object o, CancellationToken ct)
        {
            var z = o.TryGetValue<Point>(out var item);
        }

        [CreateSyncVersion]
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
""".Verify(sourceType: SourceType.Full);

#if NETCOREAPP1_0_OR_GREATER
    [Fact]
    public Task Overloads() => """
[CreateSyncVersion]
async Task ReadAsMemoryAsync(Stream stream, byte[] sampleBytes, CancellationToken ct)
    => await stream.ReadAsync(sampleBytes.AsMemory(0, 123), ct).ConfigureAwait(false);

[CreateSyncVersion]
async Task ReadAsMemoryAsync(Stream stream, byte[] sampleBytes)
    => await stream.ReadAsync(sampleBytes.AsMemory(0, 123)).ConfigureAwait(false);
""".Verify();

    [Fact]
    public Task FindsASpan() => """
class ClassThatReturnsMemoryAndSpan
{
    public Memory<byte> GetMemory(int sizeHint = 0) => throw new NotImplementedException();
    public Span<byte> GetSpan(int sizeHint = 0) => throw new NotImplementedException();
}

[CreateSyncVersion]
private async Task GetMemoryOrSpanAsync()
{
    var instance = new ClassThatReturnsMemoryAndSpan();
    Memory<byte> mem = instance.GetMemory();
    var arr = mem.Span.ToArray();
}
""".Verify();
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
    public Task DropIProgressExpressionArgument(string callArgument) => $$"""
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

[CreateSyncVersion]
public static async Task CallWithIProgressAsync()
{
    var progress = new Progress<float>();
    await WithIProgressAsync({{callArgument}});
}
""".Verify(false, true);

    [Theory]
    [InlineData("progress++;")]
    [InlineData("if (true) { progress++; }")]
    [InlineData("if (true) progress++;")]
    [InlineData("if (true) { } else progress++;")]
    [InlineData("if (false) { } else if (true) progress++;")]
    [InlineData("if (false) { } else if (true) progress++; else { }")]
    [InlineData("""
        switch (k)
        {
            case 1:
                progress++;
                break;
            default:
                progress++;
                progress++;
                break;
        }
        """)]

    public Task DropIProgressStatement(string statement) => $$"""
public static async Task WithIProgressAsync(IProgress<float>? progress = null)
{
    await Task.CompletedTask;
}

public static void WithIProgress()
{
}

static int k = 2;

[CreateSyncVersion]
public static async Task CallWithIProgressAsync()
{
    CustomProgress progress = new();

    {{statement}}

    await WithIProgressAsync(progress);
}

private sealed class CustomProgress : IProgress<float>
{
    public static CustomProgress operator ++(CustomProgress a) => a;

    public void Report(float value) => throw new NotImplementedException();
}
""".Verify(false, true);

    [Fact]
    public Task VerifyParamHandling() => $$"""
static byte[] HelperMethod(params int[] myParams) => null;
_ = HelperMethod(1, 2);
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task VerifyAsyncParamHandling() => $$"""
static async Task<byte[]> HelperMethod(params int[] myParams) => null;
_ = await HelperMethod(1, 2);
""".Verify(sourceType: SourceType.MethodBody);
}
