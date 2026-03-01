//HintName: Test.Class.MethodAsync.g.cs
void Method(global::System.Threading.CancellationToken ct = default)
{
    if (!ct.IsCancellationRequested)
    {
        ct.ThrowIfCancellationRequested();
    }
}
