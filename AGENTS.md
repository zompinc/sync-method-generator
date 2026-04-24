# Agents Guide

See README.md for project overview and consumer API.

## Build & Test

```bash
# Build
dotnet build

# Test
dotnet test

# Pack (multi-Roslyn-version NuGet package)
./Pack.ps1
```

The project uses a `.slnx` solution file: `Zomp.SyncMethodGenerator.slnx`.

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
  DiagnosticMessages.cs                ZSMGEN001-003 diagnostic descriptors
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

## Key Conventions

- Central package management (`Directory.Packages.props`) — never put versions in csproj files
- `TreatWarningsAsErrors` is enabled globally
- StyleCop + NetAnalyzers enforced; `.editorconfig` defines style rules
- File-scoped namespaces required
- Nerdbank.GitVersioning for version management (from git tags/height)
- The generator targets `netstandard2.0` for maximum host compatibility
- `EquatableArray<T>` wraps `ImmutableArray<T>` for value equality in the incremental pipeline

## Diagnostics

| ID | Description |
|----|-------------|
| ZSMGEN001 | Invalid nesting of `SYNC_ONLY` directive |
| ZSMGEN002 | `SYNC_ONLY` mixed with other symbols in `#if` condition |
| ZSMGEN003 | `SYNC_ONLY` used with `#elif` |
