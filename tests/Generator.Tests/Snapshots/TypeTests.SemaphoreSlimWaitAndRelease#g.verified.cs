//HintName: Test.Class.MethodAsync.g.cs
var semaphore = new global::System.Threading.SemaphoreSlim(1, 1);

semaphore.Wait();

try
{ 
}
finally
{
    semaphore.Release();
}
