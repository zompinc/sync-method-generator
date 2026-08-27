# Replacing hand-written twins with generated ones

Adding the generator to a new method is low risk: nothing existed before, and the compiler
checks the result. Replacing a sync method somebody wrote and shipped is different. The
generated method has to be the method it replaces, and "looks about right" is not evidence.

SharpCompress wrote up its migration in
[docs/SYNC_METHOD_GENERATION.md](https://github.com/adamhathcock/sharpcompress/blob/master/docs/SYNC_METHOD_GENERATION.md).
Worth reading before a first migration in an established codebase.

## Order of work

The generated partial and the hand-written method have the same signature, so they cannot
coexist - keeping both is a duplicate member error. That means capturing the original before
deleting it.

1. **Capture the hand-written sync method.** Copy its full text somewhere outside the tree, or
   rely on `git show HEAD:<path>` after the deletion.
1. **Delete the hand-written sync method** and attribute the async one.
1. **Build with emission on** so the generated file lands on disk:
   ```xml
   <EmitCompilerGeneratedFiles>true</EmitCompilerGeneratedFiles>
   ```
   Output goes to
   `obj/<Configuration>/<TargetFramework>/generated/Zomp.SyncMethodGenerator/Zomp.SyncMethodGenerator.SyncMethodSourceGenerator/<Namespace>.<Type>.<Method>.g.cs`.
1. **Compare the generated method against the captured one**, normalised (see below).
1. **Explain every difference before accepting it.** A difference is either a bug in the
   generator, a real behavioural difference that was hiding in the pair, or something the
   original got wrong. All three are worth knowing about. None of them are noise.

## Normalising the comparison

A direct `diff` is dominated by two differences that do not matter, and hides the ones that do:

- **`global::` qualification.** The generator fully qualifies types. Strip the prefix before
  comparing - but strip it precisely. A regex like `s/global::[A-Za-z.]*\.//` is over-greedy and
  will eat parts of expressions such as `ArgumentException.ThrowIfNull`, producing a clean diff
  that proves nothing.
- **Whitespace and line breaks.** The generator formats from the syntax tree, so wrapping
  differs from what a human typed.

Compare token sequences, or normalise whitespace to single spaces and drop the qualification
prefix only where it directly precedes a type name. Then require exact equality. On a real
migration of ten methods, "ten of ten identical after normalisation" is the result worth
reporting; anything less needs a per-method explanation.

## Generating a member instead of removing one

The trap that does not show up as a diff. Before attributing a method, check what the sync
signature would bind to today.

SharpCompress hit this with `Read(Span<byte>)`. Generating it from `ReadAsync(Memory<byte>)` did
not deduplicate anything - `Stream` already provides a `Read(Span<byte>)` shim that routes to
`Read(byte[], int, int)`. The generated override displaced the shim, which is a change in
behaviour for every caller, not a cleanup. It may well be an improvement, but it is a separate
decision from removing duplication and should be made deliberately.

Ask, for each method: is there an existing base class or interface member with this exact
signature? If yes, the generated method is an override, and the change is about behaviour.

## Scope of a first pull request

Maintainers of established libraries are being asked to accept generated code into a build they
own. A first change that converts five to ten methods in one file, with the verification shown,
is easier to accept than one that sweeps a whole project. Save the wide sweep for after the
approach has been agreed.
