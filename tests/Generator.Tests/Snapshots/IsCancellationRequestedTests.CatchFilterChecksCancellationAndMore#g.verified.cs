//HintName: Test.Class.MethodAsync.g.cs
try
{
    global::System.Threading.Thread.Sleep(1);
}
catch (global::System.OperationCanceledException) when (global::System.Threading.CancellationToken.None.IsCancellationRequested || global::System.Environment.TickCount > 0)
{
    throw;
}
