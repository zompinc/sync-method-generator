namespace Generator.Tests;

[UsesVerify]
public class SyncOnlyTests
{
    [Theory]
    [InlineData(true, "")]
    [InlineData(true, "if (!await component.AsyncDump(ct)) { }\n")]
    [InlineData(false, "\nif (!await component.AsyncDump(ct)) { }")]
    [InlineData(false, "\nif (!await component.AsyncDump(ct)) throw new InvalidOperationException(\"async exception\");")]
    public Task SimpleMacro(bool before, string additionalIgnorableCommands)
    {
        // The source code to test
        var source = $$"""
using System;
using System.Threading;
using System.Threading.Tasks;

namespace Test;

internal partial class SimpleMacro
{
    Component component = new();

    [Zomp.SyncMethodGenerator.CreateSyncVersion]
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
}
""";
        return TestHelper.Verify(source, false, true);
    }

    [Fact]
    public Task InsideEmptyIf()
    {
        // The source code to test
        var source = $$"""
using System;
using System.Threading;
using System.Threading.Tasks;

namespace Test;

internal partial class Stuff
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public async Task ExecAsync(CancellationToken ct)
    {
        if (true)
        {
#if SYNC_ONLY
            throw new InvalidOperationException("Some exception");
#endif
        }
    }
}
""";
        return TestHelper.Verify(source, false, true);
    }

    [Fact]
    public Task StatementAtTheEnd()
    {
        // The source code to test
        var source = $$"""
using System;
using System.Threading;
using System.Threading.Tasks;

namespace Test;

internal partial class Stuff
{

    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public async Task ExecAsync()
    {
#if SYNC_ONLY
#endif
        Console.Write("Done");
    }
}
""";
        return TestHelper.Verify(source, false, true);
    }

    [Fact]
    public Task DoNotRemove()
    {
        // The source code to test
        var source = """
using System;
using System.Threading;
using System.Threading.Tasks;

namespace Test;

internal partial class SimpleMacro
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public async Task ExecAsync(CancellationToken ct)
    {
#if SYMBOL1
        ArgumentNullException.ThrowIfNull(source);
#elif SYMBOL2
        ArgumentNullException.ThrowIfNull(source);
#else
        ArgumentNullException.ThrowIfNull(source);
#endif
    }
}
""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task DoNotRemoveInsideSyncOnly()
    {
        var source = """
using System;
using System.Threading;
using System.Threading.Tasks;

namespace Test;

internal partial class SimpleMacro
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public async Task ExecAsync(CancellationToken ct)
    {
#if SYNC_ONLY
#if SYMBOL
        throw new global::System.InvalidOperationException("Some exception");
#endif
#endif
        await Task.CompletedTask;
    }
}
""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task NotSyncOnly()
    {
        var source = """
using System;
using System.Threading;
using System.Threading.Tasks;

namespace Test;

internal partial class SimpleMacro
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public async Task ExecAsync(CancellationToken ct)
    {
#if !SYNC_ONLY
        Console.Write("Async");
#endif
        await Task.CompletedTask;
    }
}
""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task NotSyncOnlyInsideSyncOnly()
    {
        var source = """
using System;
using System.Threading;
using System.Threading.Tasks;

namespace Test;

internal partial class SimpleMacro
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public async Task ExecAsync(CancellationToken ct)
    {
#if SYNC_ONLY
#if !SYNC_ONLY
        Console.Write("Async");
#endif
#endif
        await Task.CompletedTask;
    }
}
""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task NestedSyncOnly()
    {
        var source = """
using System;
using System.Threading;
using System.Threading.Tasks;

namespace Test;

internal partial class SimpleMacro
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public async Task ExecAsync(CancellationToken ct)
    {
#if SYNC_ONLY
#if SYNC_ONLY
        System.Console.Write("Sync");
#endif
#endif
        await Task.CompletedTask;
    }
}
""";
        return TestHelper.Verify(source);
    }

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

        var source = $$"""
using System;
using System.Threading;
using System.Threading.Tasks;

namespace Test;

internal partial class SimpleMacro
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public async Task ExecAsync(CancellationToken ct)
    {
{{conditions}}
        await Task.CompletedTask;
    }
}
""";
        return TestHelper.Verify(source, disableUnique: true);
    }

    [Fact]
    public Task ProhibitElif()
    {
        // The source code to test
        var source = """
using System;
using System.Threading;
using System.Threading.Tasks;

namespace Test;

internal partial class SimpleMacro
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public async Task ExecAsync()
    {
#if SYNC_ONLY
        Console.Write("Async");
#elif Symbol
        Console.Write("Whatevs");
#endif
        await Task.CompletedTask;
""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task DoNotMixSymbols()
    {
        // The source code to test
        var source = """
using System;
using System.Threading;
using System.Threading.Tasks;

namespace Test;

internal partial class SimpleMacro
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public async Task ExecAsync()
    {
#if SYMBOL1 && !(SYNC_ONLY || SYMBOL2)
        System.Console.Write("Sync");
#endif
    }
}
""";
        return TestHelper.Verify(source);
    }

    [Fact]
    public Task SwitchStatementProcessesDirectives()
    {
        // The source code to test
        var source = """
using System;
using System.Threading;
using System.Threading.Tasks;

namespace Test;

internal partial class SimpleMacro
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public async Task ExecAsync()
    {
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
    }
}
""";
        return TestHelper.Verify(source);
    }
}
