//HintName: Test.Class.FibonacciAsync.g.cs
global::System.Collections.Generic.IEnumerable<int> Fibonacci(global::System.Threading.CancellationToken ct = default)
{
    var f0 = 0;
    var f1 = 1;
    yield return f0;
    yield return f1;
    while (!ct.IsCancellationRequested)
    {
        var fn = f0 + f1;
        yield return fn;
        f0 = f1;
        f1 = fn;
    }
}
