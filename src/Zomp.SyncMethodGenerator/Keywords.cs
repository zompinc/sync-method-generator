#if false
namespace Zomp.SyncMethodGenerator;

internal class Keywords
{
    // Namespace parts
    public const string Collections = "Collections";
    public const string CompilerServices = "CompilerServices";
    public const string Func = "Func";
    public const string Generic = "Generic";
    public const string System = "System";
    public const string Runtime = "Runtime";
    public const string Threading = "Threading";
    public const string Tasks = "Tasks";

    // Type names
    public const string CancellationToken = nameof(global::System.Threading.CancellationToken);
    public const string Enumerator = nameof(Span<>.Enumerator);
    public const string Memory = nameof(Memory<>);
    public const string IAsyncEnumerable = nameof(IAsyncEnumerable<>);
    public const string IAsyncEnumerator = nameof(IAsyncEnumerator<>);
    public const string IAsyncResult = nameof(global::System.IAsyncResult);
    public const string IProgress = nameof(IProgress<>);
    public const string Object = "object";
    public const string Task = nameof(Task<>);
    public const string ValueTask = nameof(ValueTask<>);

    // Members
    public const string CompletedTask = nameof(global::System.Threading.Tasks.Task.CompletedTask);
    public const string Delay = nameof(Task<>.Delay);
    public const string FromResult = nameof(Task<>.FromResult);
    public const string WaitAsync = "WaitAsync";
    public const string IsCancellationRequested = nameof(global::System.Threading.CancellationToken.IsCancellationRequested);
    public const string MoveNextAsync = nameof(IAsyncEnumerator<>.MoveNextAsync);
    public const string DisposeAsync = nameof(IAsyncEnumerator<>.DisposeAsync);
    public const string Span = nameof(Memory<>.Span);

    public const string SystemFunc = $"{System}.{Func}";
    public const string IEnumerable = $"{System}.{Collections}.{Generic}.{nameof(IEnumerable<>)}";
    public const string IEnumerator = $"{System}.{Collections}.{Generic}.{nameof(IEnumerator<>)}";
}
#endif
