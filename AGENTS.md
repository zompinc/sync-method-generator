# Agents Guide

See README.md for project overview and consumer API.

## Cloning on Windows

The snapshot file names under `tests/Generator.Tests/Snapshots` run to 199
characters, which carries a checkout past the 260 character Windows path limit
unless the clone root is unusually shallow. Git reports `Filename too long` for
each one, then `fatal: unable to checkout working tree`, and leaves a
repository with most of its files missing rather than an obvious failure.

```sh
git clone -c core.longpaths=true https://github.com/zompinc/sync-method-generator.git
```

`-c` applies the setting to this clone alone. Setting it globally is worth
doing on any Windows machine which builds .NET repositories, snapshot testing
being common enough that this is not the last repository to hit it:

```sh
git config --global core.longpaths true
```

The length is mostly intrinsic rather than careless. The generator caps its own
generated file names at 100 characters, and Verify prefixes each snapshot with
the test class and method names, which puts the floor near 155 before any
particular test is named. Renaming tests buys tens of characters; it does not
remove the limit.

Consumers installing the Claude Code plugin are unaffected, the documented
install command using a sparse checkout which never touches the snapshots.

## Claude Code plugin

`.claude/settings.json` declares this repository as a plugin marketplace and
enables the `sync-method-generator` plugin at project scope, so Claude Code
offers the skill to anyone working here. The skill lives in
`plugins/sync-method-generator/skills/sync-from-async` and is the same one
consumers install; editing it and reloading is how to try a change.

Declining the plugin costs nothing. It contributes a skill and no hooks, agents
or MCP servers.

## Build & Test

```bash
# Restore the Node tooling the pre-commit hook needs (once per clone)
pnpm install

# Build
dotnet build

# Test
dotnet test

# Pack (multi-Roslyn-version NuGet package)
./Pack.ps1
```

The project uses a `.slnx` solution file: `Zomp.SyncMethodGenerator.slnx`.

## Line endings

**Every text file in the repository is CRLF, and the build enforces it.** An
`ENDOFLINE` error fails the build for any `.cs` file with LF endings, and the
pre-commit hook runs that build, so a commit is rejected rather than merely
flagged.

This bites in ways that are easy to miss:

- `sed -i` under Git Bash rewrites the whole file with LF. A one line change
  then shows up as every line changed. Follow any `sed -i` with
  `perl -0pi -e 's/(?<!\r)\n/\r\n/g' <file>`.
- Tools which create files usually write LF. Convert before committing.
- `git diff` against a file staged in the other convention shows the entire
  file as changed. That is the diff, not the edit.

The exception is `.husky/`, which has its own `.gitattributes` forcing LF
because the hooks are shell scripts.

## Formatting

The pre-commit hook runs two checks, and CI runs the same hook as a step:

- `dotnet format --verify-no-changes` over the solution
- `pnpm format:check`, which is Prettier over `**/*.{yml,yaml,json,md}`

Prettier is deliberately scoped to those extensions, with `endOfLine: crlf` so
it does not fight the convention above. C# formatting belongs to `dotnet
format` and the analyzers. Run `pnpm format` to fix what it finds.

## Multi-Roslyn-Version Architecture

The generator ships multiple analyzer DLLs targeting different Roslyn versions so the same NuGet package works across .NET 8, 9, and 10 SDKs.

- `Directory.Build.targets` — switches `Microsoft.CodeAnalysis.CSharp` version via `RoslynVersion` MSBuild property and defines `ROSLYN_X_Y_OR_GREATER` constants
- `src/Zomp.SyncMethodGenerator/` — the generator project; `BaseOutputPath` is set per-variant so builds don't collide
- `src/Zomp.SyncMethodGenerator.Pack/` — packing-only project that gathers pre-built variant DLLs into versioned `analyzers/dotnet/roslyn4.X/cs/` NuGet paths
- `Pack.ps1` — builds all variants in parallel, then packs

Roslyn variants: `roslyn4.8` (4.8.0, .NET 8), `roslyn4.12` (4.12.0, .NET 9), `roslyn5.0` (5.0.0, .NET 10).

Use `#if ROSLYN_X_Y_OR_GREATER` guards for APIs that only exist in newer Roslyn versions (e.g., `ExtensionBlockDeclarationSyntax` is `ROSLYN_5_0_OR_GREATER` only).

## Project Structure

```text
src/Zomp.SyncMethodGenerator/          Generator (netstandard2.0)
  SyncMethodSourceGenerator.cs         Entry point — IIncrementalGenerator
  AsyncToSyncRewriter.cs               Core transformation engine (CSharpSyntaxRewriter)
  SourceGenerationHelper.cs            Output file structure and attribute definitions
  Extensions.cs                        Type-checking extensions on INamedTypeSymbol
  DiagnosticMessages.cs                ZSMGEN001-004 diagnostic descriptors
  Models/                              Data records for the pipeline
  Helpers/                             EquatableArray<T>, DirectiveStack, etc.
  Properties/                          Assembly attributes
  tools/                               MSBuild props/targets shipped in the NuGet package
src/Zomp.SyncMethodGenerator.Pack/     Packing-only project (no code)
tests/Generator.Tests/                 Unit tests (xUnit + Verify snapshot testing)
tests/GenerationSandbox.Tests/         Integration tests (real-world patterns)
```

## Transformation Pipeline

1. **Find candidates** — `ForAttributeWithMetadataName` locates `[CreateSyncVersion]` on methods or types
2. **Extract metadata** — parent class hierarchy, namespaces, configuration flags
3. **Rewrite** — `AsyncToSyncRewriter` (a `CSharpSyntaxRewriter`) traverses the syntax tree:
   - Strips `async` modifier and `await` expressions
   - Transforms return types: `Task`/`ValueTask` to `void`, `Task<T>`/`ValueTask<T>` to `T`
   - Transforms collection types: `IAsyncEnumerable<T>` to `IEnumerable<T>`
   - Transforms memory types: `Memory<T>` to `Span<T>` (except in arrays)
   - Removes `CancellationToken` and `IProgress<T>` parameters (configurable)
   - Renames method calls: strips `Async` suffix
   - Handles special methods: `Task.FromResult(x)` to `x`, `Task.Delay()` to `Thread.Sleep()`
   - Processes `#if SYNC_ONLY` / `#if !SYNC_ONLY` directives
4. **Emit** — `SourceGenerationHelper` wraps the rewritten method in namespace/class structure

## Testing Conventions

- **Snapshot testing** with Verify.SourceGenerators — test inputs are inline C# strings, outputs are `.verified.cs` files in `Snapshots/`
- Test pattern: `[Fact] public Task TestName() => "source code".Verify();`
- Snapshot files: `{TestClass}.{TestName}[.Platform].g.verified.cs`
- Tests compile against real framework assemblies via `TestHelper`
- `TestHelper` fails a test whose generated code does not compile, before any
  snapshot is compared, and reports the compiler errors

### Choosing how the source is wrapped

`Verify` takes a `SourceType`, and the default is the least noisy one:

| SourceType            | Wraps the source in                                                 | Use for                                     |
| --------------------- | ------------------------------------------------------------------- | ------------------------------------------- |
| `ClassBody` (default) | `namespace Test; partial class Class { ... }`                       | most tests                                  |
| `StaticClassBody`     | the same, but a static class                                        | extension methods                           |
| `MethodBody`          | an `async Task MethodAsync(CancellationToken ct)` inside that class | a few statements                            |
| `Full`                | nothing, the source is the whole file                               | namespaces, using directives, several types |

Prefer the smallest wrapper the test needs, and reach for `Full` only when the
test genuinely needs file level syntax.

One trap worth knowing: the test compilation has global usings, and global
usings apply to generated files too. A bug about a type resolving through a
_file scoped_ `using` in the source but not in the generated file therefore
cannot be reproduced with anything but `Full` - with a smaller wrapper the
global using resolves the type and the bug disappears.

### Accepting snapshots

A test with no snapshot, or one whose output changed, writes a `.received.`
file next to the expected `.verified.` one. To accept it, copy it over the
verified name and drop the framework infix:

```text
UnitTests.Example.DotNet10_0#Test.Class.MethodAsync.g.received.cs
UnitTests.Example#Test.Class.MethodAsync.g.verified.cs
```

Output is normally identical across target frameworks, so one verified file
without the infix serves both. Keep the infix only where the frameworks really
differ. Delete stray `.received.` files before committing; they are ignored by
git but confusing to leave behind.

### Fixing a bug

Land the failing test first, as its own commit, then the fix. The test commit
should fail for the reason the issue describes - a compiler error in the
generated code, or a snapshot recording the output the issue asks for - so that
the fix commit demonstrably changes something. Verify this rather than assuming
it: check out the test commit alone and watch it fail.

## Key Conventions

- Central package management (`Directory.Packages.props`) — never put versions in csproj files
- `Microsoft.CodeAnalysis.CSharp` is pinned to exactly `[5.0.0]`. That is not
  staleness: it sets the lowest Roslyn a consumer needs, and the variant builds
  override it per variant. Raising it raises the minimum SDK for everyone
- `TreatWarningsAsErrors` is enabled globally
- StyleCop + NetAnalyzers enforced; `.editorconfig` defines style rules
- File-scoped namespaces required
- Nerdbank.GitVersioning for version management (from git tags/height)
- The generator targets `netstandard2.0` for maximum host compatibility
- `EquatableArray<T>` wraps `ImmutableArray<T>` for value equality in the incremental pipeline

## Diagnostics

| ID        | Description                                             |
| --------- | ------------------------------------------------------- |
| ZSMGEN001 | Invalid nesting of `SYNC_ONLY` directive                |
| ZSMGEN002 | `SYNC_ONLY` mixed with other symbols in `#if` condition |
| ZSMGEN003 | `SYNC_ONLY` used with `#elif`                           |
| ZSMGEN004 | `Task.WhenAll` or `Task.WhenAny` has no sync equivalent |

A new descriptor must also be listed in `AnalyzerReleases.Unshipped.md`, or
`RS2000` fails the build. An error severity diagnostic suppresses the generated
file for that method, which is usually what you want: emitting code known to be
wrong is worse than emitting nothing.
