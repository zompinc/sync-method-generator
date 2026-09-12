namespace Zomp.SyncMethodGenerator.Cloning;

/// <summary>
/// Emits cloned methods, each into a file of its own, and reports those which cannot be emitted.
/// </summary>
internal static class ClonedMethodOutput
{
    /// <summary>
    /// Longest generated file name before the scope is shortened. Emitted files sit under
    /// {project}\obj\{configuration}\{framework}\generated\{generator assembly}\{generator},
    /// which alone is around a hundred characters before the project path is counted.
    /// </summary>
    private const int MaxFileNameLength = 100;

    /// <summary>
    /// Adds the output of the generator: a file for each method, and the diagnostics of each.
    /// </summary>
    /// <param name="context">Context of the generator.</param>
    /// <param name="methods">Methods to emit.</param>
    /// <param name="collision">Reported against each method whose signature another method also produces.</param>
    public static void Register(
        IncrementalGeneratorInitializationContext context,
        IncrementalValuesProvider<ClonedMethod> methods,
        DiagnosticDescriptor collision)
    {
        var sourceTexts = methods
            .Select(static (m, _) => GenerateSource(m))
            .WithTrackingName("GenerateSource");

        // Signatures produced more than once. Emitting them all would declare the same member
        // twice, so the methods behind them are reported instead of generated.
        var collidingSignatures = methods
            .Select(static (m, _) => m.Signature?.Key)
            .Collect()
            .Select(static (keys, _) => MethodSignature.FindCollisions(keys));

        context.RegisterSourceOutput(
            sourceTexts.Combine(collidingSignatures),
            (spc, pair) =>
            {
                var (source, colliding) = pair;

                foreach (var diagnostic in source.Method.Diagnostics)
                {
                    spc.ReportDiagnostic(diagnostic);
                }

                if (source.Method.Signature is { } signature
                    && colliding.AsImmutableArray().Contains(signature.Key, StringComparer.Ordinal))
                {
                    spc.ReportDiagnostic(signature.ToDiagnostic(collision));
                    return;
                }

                if (!source.Method.HasErrors)
                {
                    spc.AddSource(source.Path, SourceText.From(source.Content, Encoding.UTF8));
                }
            });
    }

    private static (ClonedMethod Method, string Path, string Content) GenerateSource(ClonedMethod m)
    {
        var location = m.Location;

        var scope = $"{string.Join(".", location.Namespaces)}" +
            $".{string.Join(".", location.Parents.Select(static p => p.ScopeName))}" +
            (location.IsCSharp14Extension ? ".ext" : string.Empty);

        var method = m.MethodName + (location.Index == 1 ? string.Empty : "_" + location.Index);

        var sourcePath = BuildFileName(scope, method);

        var source = ClonedMethodSource.Generate(m);

        return (m, sourcePath, source);
    }

    /// <summary>
    /// Builds the file name a generated method is emitted under, shortening it when the
    /// namespace and containing type chain make it long enough to be a problem. Emitted files
    /// live several directories deep inside the intermediate output path, so a long name here
    /// can carry the full path past the limit the file system accepts.
    /// </summary>
    /// <remarks>
    /// The scope is what gets shortened, since the method name is what someone reads the file
    /// name for. A hash of the untruncated name keeps distinct methods in distinct files.
    /// </remarks>
    private static string BuildFileName(string scope, string method)
    {
        const string extension = ".g.cs";

        var fileName = $"{scope}.{method}{extension}";

        if (fileName.Length <= MaxFileNameLength)
        {
            return fileName;
        }

        var hash = Hash(fileName);

        // separator, hash, dot, extension
        var fixedLength = 1 + hash.Length + 1 + extension.Length;

        var forScope = MaxFileNameLength - fixedLength - method.Length;

        if (forScope > 0)
        {
            return $"{scope[..Math.Min(scope.Length, forScope)]}_{hash}.{method}{extension}";
        }

        // The method name alone fills the budget, so it has to be shortened as well.
        return $"_{hash}.{method[..(MaxFileNameLength - fixedLength)]}{extension}";
    }

    /// <summary>
    /// FNV-1a. Short, dependency free, and stable across processes and runtimes, which
    /// <see cref="string.GetHashCode()"/> is not.
    /// </summary>
    private static string Hash(string value)
    {
        const uint offsetBasis = 2166136261;
        const uint prime = 16777619;

        var hash = offsetBasis;

        unchecked
        {
            foreach (var c in value)
            {
                hash ^= c;
                hash *= prime;
            }
        }

        return hash.ToString("x8", System.Globalization.CultureInfo.InvariantCulture);
    }
}
