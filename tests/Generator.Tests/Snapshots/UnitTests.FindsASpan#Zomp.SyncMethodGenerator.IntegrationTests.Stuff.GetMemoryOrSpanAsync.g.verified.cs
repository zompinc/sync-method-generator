﻿//HintName: Zomp.SyncMethodGenerator.IntegrationTests.Stuff.GetMemoryOrSpanAsync.g.cs
// <auto-generated/>
#nullable enable
namespace Zomp.SyncMethodGenerator.IntegrationTests;
internal partial class Stuff
{
    private void GetMemoryOrSpan()
    {
        var instance = new global::Zomp.SyncMethodGenerator.IntegrationTests.Stuff.ClassThatReturnsMemoryAndSpan();
        global::System.Span<byte> mem = instance.GetSpan();
        var arr = mem.ToArray();
    }
}
