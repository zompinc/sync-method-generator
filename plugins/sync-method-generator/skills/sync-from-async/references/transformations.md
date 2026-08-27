# What the rewriter changes

Reference for deciding whether a given async method translates cleanly. Source of truth is the
project README and the snapshot tests under `tests/Generator.Tests/Snapshots/`.

## Declaration

- The `async` modifier is removed.
- The `Async` suffix is removed from the method name.
- The `CreateSyncVersionAttribute` is removed from the generated method.
- `#nullable enable` is emitted when the language version is 8 or above, unless
  `OmitNullableDirective` is set.

## Types

| From                                                        | To                             |
| ----------------------------------------------------------- | ------------------------------ |
| `Task`, `ValueTask`                                         | `void`                         |
| `Task<T>`, `ValueTask<T>`                                   | `T`                            |
| `Func<Task>`                                                | `Action`                       |
| `Func<Task<T>>`                                             | `Func<T>`                      |
| `IAsyncEnumerable<T>`                                       | `IEnumerable<T>`               |
| `IAsyncEnumerator<T>`                                       | `IEnumerator<T>`               |
| `ConfiguredCancelableAsyncEnumerable<T>.Enumerator`         | `IEnumerator<T>`               |
| `ConfiguredCancelableAsyncEnumerable<T>.GetAsyncEnumerator` | `IEnumerable<T>.GetEnumerator` |
| `Memory<T>`                                                 | `Span<T>`                      |
| `ReadOnlyMemory<T>`                                         | `ReadOnlySpan<T>`              |

`Memory<T>` and `ReadOnlyMemory<T>` are left alone when they appear as a type argument of a
collection - a `ref struct` cannot be an array element type, so the substitution would not
compile.

## Parameters

- `CancellationToken` parameters are removed, unless `PreserveCancellationToken` is set.
- `IProgress<T>` parameters are removed, unless `PreserveProgress` is set.

## Statements and invocations

- `await` is removed, including from `await foreach`.
- `ConfigureAwait` is removed from tasks and from async enumerations, including standalone
  `ConfigureAwait` statements.
- `WaitAsync` and `WithCancellation` calls are removed.
- Invocations ending in `Async` are rewritten to call the sync overload:
  `MoveNextAsync()` becomes `MoveNext()`.
- Async invocations without an `Async` suffix are removed.
- `CancellationToken` arguments are dropped from calls.
- `IProgress<T>.Report(T)` calls are removed unless `PreserveProgress` is set.
- `Memory<T>.Span` property accesses are removed, the value already being a span.
- `await Task.FromResult(value)` becomes `value`.
- `await Task.Delay(value)` becomes `Thread.Sleep(value)`.
- Any invocation returning `ConfiguredCancelableAsyncEnumerable<T>` becomes `GetEnumerator()`.

## Documentation

XML documentation comments are carried over. A `<returns>` block is dropped when the sync method
returns nothing. `using` directives from the source file are carried into the generated file, so
`cref`s that relied on them still resolve.

## Where it stops

The rewriter is syntactic and semantic, not a general translator. Constructs it does not know
about are copied through unchanged, which usually surfaces as a compile error in the generated
file rather than as silently wrong code. Read the generated output before trusting it - see the
verification section of the skill.
