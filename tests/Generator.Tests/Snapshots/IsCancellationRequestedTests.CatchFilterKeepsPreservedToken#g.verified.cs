//HintName: Test.Class.MethodAsync.g.cs
public void Method(global::System.Threading.CancellationToken ct)
{
    try
    {
        global::System.Threading.Thread.Sleep(1);
    }
    catch (global::System.OperationCanceledException) when (ct.IsCancellationRequested)
    {
        throw;
    }
}
