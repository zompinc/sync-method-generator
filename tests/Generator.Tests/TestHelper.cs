using System.Data.Common;
using System.Text.RegularExpressions;
using Zomp.SyncMethodGenerator;
using static Generator.Tests.ModuleInitializer;

namespace Generator.Tests;

/// <summary>
/// Helps set up the test cases.
/// </summary>
public static partial class TestHelper
{
    private const string GlobalUsingsSource = """
global using global::System;
global using global::System.Buffers;
global using global::System.Collections.Generic;
global using global::System.Data;
global using global::System.Data.Common;
global using global::System.Drawing;
global using global::System.IO;
global using global::System.Linq;
#if NET7_0_OR_GREATER
global using global::System.Numerics;
#endif
global using global::System.Reflection;
global using global::System.Runtime.CompilerServices;
global using global::System.Threading;
global using global::System.Threading.Tasks;
global using global::Zomp.SyncMethodGenerator;
""";

    private static readonly string[] PreprocessorSymbols
#if NETFRAMEWORK
        = Array.Empty<string>();
#else
        =
    [
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
    ];
#endif

    internal static Task Verify(
        this string source,
        bool uniqueForFramework = false,
        bool disableUnique = false,
        SourceType sourceType = SourceType.ClassBody,
        LanguageVersion languageVersion = LanguageVersion.Preview,
        params object?[] parameters)
    {
        var parseOptions = CSharpParseOptions.Default
            .WithLanguageVersion(languageVersion)
            .WithPreprocessorSymbols(PreprocessorSymbols);

        if (sourceType != SourceType.Full)
        {
            if (sourceType == SourceType.MethodBody)
            {
                source = $$"""
[CreateSyncVersion]
async Task MethodAsync(CancellationToken ct)
{
{{ChangeIndentation(source, InsertIndentation)}}
}
""";
            }

            source = ChangeIndentation(source, InsertIndentation);
            source = $$"""
namespace Test;
partial class Class
{
{{source}}
}
""";
        }

        SyntaxTree syntaxTree = CSharpSyntaxTree.ParseText(source, parseOptions);
        SyntaxTree globalUsings = CSharpSyntaxTree.ParseText(GlobalUsingsSource, parseOptions);

        // SyntaxTree syntaxTree = CSharpSyntaxTree.ParseText(source);
        var linqAssembly = typeof(Enumerable).Assembly.Location;
        var locations = new List<string>
        {
            typeof(IAsyncEnumerable<>).Assembly.Location,
            typeof(DbDataReader).Assembly.Location,
            typeof(ValueTask<>).Assembly.Location,
            typeof(System.Drawing.Point).Assembly.Location,
            typeof(object).Assembly.Location,
            typeof(Console).Assembly.Location,
            typeof(Memory<>).Assembly.Location,
            typeof(System.Buffers.MemoryPool<>).Assembly.Location,
            typeof(Queue<>).Assembly.Location,
            typeof(LinkedListNode<>).Assembly.Location,
#if NET6_0_OR_GREATER
            typeof(AsyncEnumerable).Assembly.Location,
#endif
            linqAssembly,
        };

        var directory = Path.GetDirectoryName(linqAssembly);
        var runtimeLocation = directory is null ? null : Path.Combine(directory, "System.Runtime.dll");
        if (runtimeLocation is not null && File.Exists(runtimeLocation))
        {
            locations.Add(runtimeLocation);
        }

        var distinct = locations.Distinct().ToArray();

        IEnumerable<PortableExecutableReference> references = distinct
            .Select(l => MetadataReference.CreateFromFile(l));

        var compilation = CSharpCompilation.Create(
            assemblyName: "Tests",
            options: new(OutputKind.DynamicallyLinkedLibrary),
            syntaxTrees: [syntaxTree, globalUsings],
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

    internal static string InsertIndentation(string s) => s.Length == 0 || s[0] == '#' ? s : $"    {s}";

    internal static string RemoveIndentation(string s) => s.StartsWith("    ", StringComparison.Ordinal) ? s[4..] : s;

    internal static string ChangeIndentation(string source, Func<string, string> func)
    {
#if NET7_0_OR_GREATER
        var lines = NewLineRegex().Split(source);
#else
        var lines = Regex.Split(source, "\r\n");
#endif
        var linesWithIndentation = string.Join("\r\n", lines.Select(func));
        return linesWithIndentation;
    }

#if NET7_0_OR_GREATER
    [GeneratedRegex("\r\n")]
    private static partial Regex NewLineRegex();
#endif
}
