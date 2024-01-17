namespace Generator.Tests;

public class SyncOnlyTests
{
    [Theory]
    [InlineData(true, "")]
    [InlineData(true, "if (!await component.AsyncDump(ct)) { }\n")]
    [InlineData(false, "\nif (!await component.AsyncDump(ct)) { }")]
    [InlineData(false, "\nif (!await component.AsyncDump(ct)) throw new InvalidOperationException(\"async exception\");")]
    public Task SimpleMacro(bool before, string additionalIgnorableCommands) => $$"""
Component component = new();

[CreateSyncVersion]
public async Task ExecAsync(CancellationToken ct)
{
{{(before ? additionalIgnorableCommands : string.Empty)}}#if SYNC_ONLY
    throw new InvalidOperationException("Some exception");
#endif{{(before ? string.Empty : additionalIgnorableCommands)}}
}

class Component
{
    internal async Task<bool> AsyncDump(CancellationToken token) {
        return await Task.FromResult(false);
    }
}
""".Verify(false, true);

    [Fact]
    public Task InsideEmptyIf() => $$"""
if (true)
{
#if SYNC_ONLY
    throw new InvalidOperationException("Some exception");
#endif
}
""".Verify(false, true, sourceType: SourceType.MethodBody);

    [Fact]
    public Task StatementAtTheEnd() => $$"""

#if SYNC_ONLY
#endif
Console.Write("Done");
""".Verify(false, true, sourceType: SourceType.MethodBody);

    [Fact]
    public Task DoNotRemove() => """
#if SYMBOL1
ArgumentNullException.ThrowIfNull(source);
#elif SYMBOL2
ArgumentNullException.ThrowIfNull(source);
#else
ArgumentNullException.ThrowIfNull(source);
#endif
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task DoNotRemoveInsideSyncOnly() => """
#if SYNC_ONLY
#if SYMBOL
throw new global::System.InvalidOperationException("Some exception");
#endif
#endif
await Task.CompletedTask;
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task NotSyncOnly() => """
#if !SYNC_ONLY
Console.Write("Async");
#endif
await Task.CompletedTask;
""".Verify(sourceType: SourceType.MethodBody);

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public Task NotSyncOnlyInsideSyncOnly(bool atLastToken)
    {
        var lastStatement = string.Empty;
        if (!atLastToken)
        {
            lastStatement = "await Task.CompletedTask;";
        }

        return $$"""
if (true) { }
#if SYNC_ONLY
#if !SYNC_ONLY
Console.Write("Async");
#endif
#endif
{{lastStatement}}
""".Verify(disableUnique: true, sourceType: SourceType.MethodBody);
    }

    [Fact]
    public Task NestedSyncOnly() => """
#if SYNC_ONLY
#if SYNC_ONLY
System.Console.Write("Sync");
#endif
#endif
await Task.CompletedTask;
""".Verify(sourceType: SourceType.MethodBody);

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public Task NotSyncOnlyFollowedBySyncOnly(bool syncFirst, bool useElse)
    {
        var conditions = syncFirst switch
        {
            true when useElse => """
#if SYNC_ONLY
System.Console.Write("Sync");
#else
Console.Write("Async");
#endif
""",
            true when !useElse => """
#if SYNC_ONLY
System.Console.Write("Sync");
#endif
#if !SYNC_ONLY
Console.Write("Async");
#endif
""",
            false when useElse => """
#if !SYNC_ONLY
Console.Write("Async");
#else
System.Console.Write("Sync");
#endif
""",
            _ => """
#if SYNC_ONLY
System.Console.Write("Sync");
#endif
#if !SYNC_ONLY
Console.Write("Async");
#endif
""",
        };

        return $$"""
{{conditions}}
await Task.CompletedTask;
""".Verify(disableUnique: true, sourceType: SourceType.MethodBody);
    }

    [Fact]
    public Task ProhibitElif() => """
#if SYNC_ONLY
Console.Write("Async");
#elif Symbol
Console.Write("Whatevs");
#endif
await Task.CompletedTask;
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task DoNotMixSymbols() => """
#if SYMBOL1 && !(SYNC_ONLY || SYMBOL2)
System.Console.Write("Sync");
#endif
""".Verify(sourceType: SourceType.MethodBody);

    [Fact]
    public Task SwitchStatementProcessesDirectives() => """
var i = 0;
switch (i)
{
    case 0:
#if !SYNC_ONLY
        Console.Write("Async");
#endif
        break;
    case 2:
#if SYNC_ONLY
        System.Console.Write("Sync");
#endif
        break;
    case 3:
#if SYMBOL
        System.Console.Write("Symbol");
#endif
        break;
}

await Task.CompletedTask;
""".Verify(sourceType: SourceType.MethodBody);
}
