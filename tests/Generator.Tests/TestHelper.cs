using Zomp.SyncMethodGenerator;
using static Generator.Tests.ModuleInitializer;

namespace Generator.Tests;

public static class TestHelper
{
    private static readonly string[] PreprocessorSymbols
#if NETFRAMEWORK
        = Array.Empty<string>();
#else
        = new string[]
    {
#if NET7_0
        "NET7_0",
#endif
#if NET7_0_OR_GREATER
        "NET7_0_OR_GREATER",
#endif
#if NET6_0
        "NET6_0",
#endif
#if NET6_0_OR_GREATER
        "NET6_0_OR_GREATER",
#endif
    };
#endif

    public static Task Verify(string source, bool uniqueForFramework = false, bool disableUnique = false, params object?[] parameters)
    {
        var parseOptions = CSharpParseOptions.Default
            .WithLanguageVersion(LanguageVersion.Preview)
            .WithPreprocessorSymbols(PreprocessorSymbols);

        SyntaxTree syntaxTree = CSharpSyntaxTree.ParseText(source, parseOptions);

        // SyntaxTree syntaxTree = CSharpSyntaxTree.ParseText(source);
        var locations = new[]
        {
            typeof(IAsyncEnumerable<>).Assembly.Location,
            typeof(ValueTask<>).Assembly.Location,
            typeof(System.Drawing.Point).Assembly.Location,
            typeof(object).Assembly.Location,
            typeof(Console).Assembly.Location,
            typeof(Memory<>).Assembly.Location,
            typeof(Queue<>).Assembly.Location,
            typeof(LinkedListNode<>).Assembly.Location,
        };

        var distinct = locations.Distinct().ToArray();

        IEnumerable<PortableExecutableReference> references = distinct
            .Select(l => MetadataReference.CreateFromFile(l));

        CSharpCompilation compilation = CSharpCompilation.Create(
            assemblyName: "Tests",
            options: new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            syntaxTrees: new[] { syntaxTree },
            references: references);

        var generator = new SyncMethodSourceGenerator();

        GeneratorDriver driver = CSharpGeneratorDriver.Create(generator).WithUpdatedParseOptions(parseOptions);

        driver = driver.RunGenerators(compilation);

        // Ensure that at least two sources are generated
        var results = driver.GetRunResult();
        if (results.Diagnostics.Length == 0 && results.GeneratedTrees.Length < 2)
        {
            throw new InvalidOperationException("Nothing generated");
        }

        var target = new RunResultWithIgnoreList
        {
            Result = driver.GetRunResult(),
            IgnoredFiles = { $"{SyncMethodSourceGenerator.CreateSyncVersionAttribute}.g.cs" },
        };

        var verifier = Verifier
            .Verify(target)
            .UseDirectory("Snapshots");

        if (uniqueForFramework)
        {
            verifier = verifier.UniqueForTargetFrameworkAndVersion();
        }

        if (disableUnique)
        {
            verifier = verifier.DisableRequireUniquePrefix();
        }

        if (parameters is { Length: > 0 })
        {
            verifier = verifier.UseParameters(parameters);
        }

        return verifier;
    }
}
