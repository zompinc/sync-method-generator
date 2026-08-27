---
name: sync-from-async
description: >
  Generate the synchronous half of a C# method pair from the async half with the
  Zomp.SyncMethodGenerator source generator, instead of hand-writing and maintaining both.
  Use whenever a .NET type needs sync and async versions of the same method: adding a sync
  overload beside an async one, editing a file where `Foo` and `FooAsync` share a body, or
  removing the duplication between hand-written twins. Also use when asked to "write the sync
  version of this method", "keep sync and async in step", or "stop duplicating sync and
  async". DO NOT USE FOR: turning sync code async, calling async code from a sync context
  (`GetAwaiter().GetResult()`), or languages other than C#.
---

# Sync methods generated from async ones

`Zomp.SyncMethodGenerator` is a Roslyn source generator. You write the async method, attribute
it, and the generator emits the sync twin into a partial of the same type: `await` and `async`
removed, `Task<T>` unwrapped to `T`, `IAsyncEnumerable<T>` to `IEnumerable<T>`,
`ReadOnlyMemory<T>` to `ReadOnlySpan<T>`, `CancellationToken` and `IProgress<T>` parameters
dropped, `ConfigureAwait` calls removed, and `FooAsync()` invocations rewritten to `Foo()`.

```cs
[Zomp.SyncMethodGenerator.CreateSyncVersion]
static async Task WriteAsync(ReadOnlyMemory<byte> buffer, Stream stream, CancellationToken ct)
    => await stream.WriteAsync(buffer, ct).ConfigureAwait(false);

// generated:
static void Write(ReadOnlySpan<byte> buffer, Stream stream)
    => stream.Write(buffer);
```

The point is that the two halves cannot drift. A bug fixed in the async method is fixed in the
sync one at the next build.

## Check that it fits before proposing it

Run through this first. The generator is a good answer for most sync/async pairs and a bad
answer for a few, and the bad cases are cheaper to spot now than after a migration.

1. **Both halves must live in the same type.** The generated method lands in a partial of the
   type declaring the async method. If the hand-written sync version lives in a different class
   (a `FooSync` helper, a separate `SyncExtensions`), the generator adds a member rather than
   replacing one. Not a fit without moving code first.
1. **The type must be `partial`**, along with every type enclosing it.
1. **The sync half must be a mechanical translation of the async half.** If it uses a different
   algorithm, different locking, a sync-only fast path, or different error handling, either
   express the difference with `SYNC_ONLY` (see below) or leave the pair alone. Do not flatten a
   real behavioural difference into a generated method.
1. **The async method should be named `FooAsync`.** The generated name is the source name with
   the `Async` suffix removed; without the suffix the generated method collides with the
   original and the build fails.
1. **Watch for a generated method that adds a member instead of removing a duplicate.** If the
   sync signature you are about to generate is already satisfied by a base class or interface
   shim, generating it changes which member callers bind to. That is a behaviour change, not a
   deduplication. See [references/migrating-existing-twins.md](references/migrating-existing-twins.md).

## Set it up

```sh
dotnet add package Zomp.SyncMethodGenerator
```

`PrivateAssets="all"` keeps the generator out of the package your library ships:

```xml
<PackageReference Include="Zomp.SyncMethodGenerator" Version="2.0.42" PrivateAssets="all" />
```

With Central Package Management, the version goes in `Directory.Packages.props`. If most
projects in the solution need it, a single `GlobalPackageReference` is tidier than a
`PackageReference` per project - it carries `PrivateAssets="all"` implicitly:

```xml
<GlobalPackageReference Include="Zomp.SyncMethodGenerator" Version="2.0.42" />
```

The generator targets .NET Standard 2.0 and needs no runtime dependency on the consuming side.

## Write the async method

Attribute a single method, or attribute the type to generate for every async method in it:

```cs
[Zomp.SyncMethodGenerator.CreateSyncVersion]
partial class Reader
{
    async Task<int> ReadAsync(...) { ... }

    [Zomp.SyncMethodGenerator.SkipSyncVersion]
    async Task NoSyncCounterpartAsync(...) { ... }
}
```

Attribute properties, all defaulting to `false`:

| Property                    | Effect                                                                      |
| --------------------------- | --------------------------------------------------------------------------- |
| `PreserveCancellationToken` | Keeps `CancellationToken` parameters instead of dropping them               |
| `PreserveProgress`          | Keeps `IProgress<T>` parameters and `Report` calls instead of dropping them |
| `OmitNullableDirective`     | Suppresses the `#nullable enable` the generator emits on C# 8 and above     |

XML documentation is carried over and adjusted: a `<returns>` block is dropped when the sync
method returns nothing, and the source file's `using` directives are carried into the generated
file so `cref`s still resolve.

The full transformation table is in
[references/transformations.md](references/transformations.md). Read it when you need to know
whether a particular construct survives the rewrite.

## Code that should only run in one half

Wrap it in a `SYNC_ONLY` conditional. The symbol must never actually be defined anywhere - the
generator reads the directive rather than the compiler:

```cs
[Zomp.SyncMethodGenerator.CreateSyncVersion]
public async Task FlushAsync(CancellationToken ct)
{
#if SYNC_ONLY
    Thread.Sleep(BackoffMilliseconds);
#endif
    await Task.CompletedTask;
}
```

`#if !SYNC_ONLY` marks async-only code. The block is copied verbatim, so fully qualify anything
that relies on a `using` the generated file might not have. `SYNC_ONLY` cannot be combined with
other symbols in one condition and has no `#elif`.

## Verify what came out

Do not assume the generated method is what you expected - read it. Turn on emission:

```xml
<EmitCompilerGeneratedFiles>true</EmitCompilerGeneratedFiles>
```

Build, then find the file under
`obj/<Configuration>/<TargetFramework>/generated/Zomp.SyncMethodGenerator/Zomp.SyncMethodGenerator.SyncMethodSourceGenerator/`.
It is named `<Namespace>.<Type>.<Method>.g.cs`.

When replacing a hand-written sync method, compare the two mechanically rather than by eye.
[references/migrating-existing-twins.md](references/migrating-existing-twins.md) has the
workflow, including how to normalise the generator's `global::` qualification so the comparison
means something.

## When the output is wrong

The rewriter handles a large surface but not an unbounded one. If the generated method does not
compile or does not match the hand-written one for a reason you cannot explain, that is worth an
issue at https://github.com/zompinc/sync-method-generator/issues rather than a workaround -
several of the released fixes came from exactly this, reported by libraries adopting it.
