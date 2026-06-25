//HintName: Test.Class.MethodAsync.g.cs
semaphore.Wait();

try
{
    global::System.Threading.Thread.Sleep(100);
}
finally
{
    semaphore.Release();
}
