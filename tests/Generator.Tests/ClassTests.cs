namespace Generator.Tests;

public class ClassTests
{
    [Fact]
    public Task ConvertClass() => $$"""
namespace Test;

interface IAsyncFileSystem
{
    Task<string> ReadAllTextAsync(string path);
    
    Task WriteAllTextAsync(string path, string contents);
}

interface IFileSystem : IAsyncFileSystem
{
    string ReadAllText(string path);
    
    void WriteAllText(string path, string contents);
}

[Zomp.SyncMethodGenerator.CreateSyncVersion(Name = "FileSystem")]
internal class AsyncFileSystem : IAsyncFileSystem
{
    public virtual Task<string> ReadAllTextAsync(string path) => Task.FromResult("");
    
    public virtual Task WriteAllTextAsync(string path, string contents) => Task.CompletedTask;
}

internal partial class FileSystem : AsyncFileSystem, IFileSystem
{
    public override Task<string> ReadAllTextAsync(string path) => Task.FromResult(ReadAllText(path));
    
    public override Task WriteAllTextAsync(string path, string contents)
    {
        WriteAllText(path, contents);
        return Task.CompletedTask;
    }
}
""".Verify(false, true);
}
