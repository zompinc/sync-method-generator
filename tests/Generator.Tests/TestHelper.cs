using Basic.Reference.Assemblies;
using System.Text.RegularExpressions;
using Zomp.SyncMethodGenerator;
using static Generator.Tests.ModuleInitializer;

namespace Generator.Tests;

/// <summary>
/// Helps set up the test cases.
/// </summary>
internal static partial class TestHelper
{
    private const string GlobalUsingsSource = """
global using global::System;
global using global::System.Buffers;
global using global::System.Collections.Generic;
global using global::System.Data;
global using global::System.Data.Common;
global using global::System.Diagnostics;
global using global::System.Drawing;
global using global::System.IO;
global using global::System.Linq;
#if NET8_0_OR_GREATER
global using global::System.Numerics;
#endif
global using global::System.Reflection;
global using global::System.Runtime.CompilerServices;
global using global::System.Text;
global using global::System.Threading;
global using global::System.Threading.Tasks;
global using global::System.Xml;
global using global::Zomp.SyncMethodGenerator;
""";

    private static readonly string[] PreprocessorSymbols
#if NETFRAMEWORK
        = [];
#else
        =
    [
#if NET8_0
        "NET8_0",
#endif
#if NET8_0_OR_GREATER
        "NET8_0_OR_GREATER",
#endif
    ];
#endif

    internal static Task Verify(
        this string source,
        bool uniqueForFramework = false,
        bool disableUnique = false,
        SourceType sourceType = SourceType.ClassBody,
        LanguageVersion languageVersion = LanguageVersion.Preview,
        DocumentationMode documentationMode = DocumentationMode.Parse,
        params object?[] parameters)
    {
        // A project which does not produce an XML documentation file compiles with
        // DocumentationMode.None, and its documentation comments are then ordinary comment
        // trivia rather than the structured kind. The default here parses them, so a test has
        // to ask for None to see what those projects see.
        var parseOptions = CSharpParseOptions.Default
            .WithLanguageVersion(languageVersion)
            .WithDocumentationMode(documentationMode)
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
{{(sourceType == SourceType.StaticClassBody ? "static " : string.Empty)}}partial class Class
{
{{source}}
}
""";
        }

        var syntaxTree = CSharpSyntaxTree.ParseText(source, parseOptions);
        var globalUsings = CSharpSyntaxTree.ParseText(GlobalUsingsSource, parseOptions);

        List<SyntaxTree> syntaxTrees = [syntaxTree];

        if (languageVersion >= LanguageVersion.CSharp10)
        {
            syntaxTrees.Add(globalUsings);
        }

#if NET8_0_OR_GREATER
        var assemblies = new[]
        {
            typeof(Microsoft.EntityFrameworkCore.EntityFrameworkQueryableExtensions).Assembly,
        };
#else
        IEnumerable<System.Reflection.Assembly> assemblies = [];
#endif
        var compilation = CSharpCompilation.Create(
            assemblyName: "Tests",
            options: new(OutputKind.DynamicallyLinkedLibrary, nullableContextOptions: languageVersion >= LanguageVersion.CSharp8 ? NullableContextOptions.Enable : NullableContextOptions.Disable),
            syntaxTrees: syntaxTrees,
            references: Net100.References.All.Concat(assemblies.Select(a => MetadataReference.CreateFromFile(a.Location))));

        var generator = new SyncMethodSourceGenerator();

        var driver = CSharpGeneratorDriver.Create(generator).WithUpdatedParseOptions(parseOptions);

        driver = driver.RunGenerators(compilation);

        // Ensure that at least two sources are generated
        var results = driver.GetRunResult();
        if (results.Diagnostics.Length == 0 && results.GeneratedTrees.Length < 3)
        {
            throw new InvalidOperationException("Nothing generated");
        }

        // Ensure compilation has no errors
        var generatedCompilation = compilation.AddSyntaxTrees(results.GeneratedTrees);
        var diagnostics = generatedCompilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (diagnostics.Length > 0)
        {
            throw new InvalidOperationException(
                "Compilation errors:\n" + string.Join("\n", diagnostics.Select(d => d.ToString())));
        }

        EnsureIdentityClonesCompile(compilation, parseOptions);

        var target = new RunResultWithIgnoreList
        {
            Result = driver.GetRunResult(),
            IgnoredFiles = { $"{SyncMethodSourceGenerator.CreateSyncVersionAttribute}.g.cs", $"{SyncMethodSourceGenerator.SkipSyncVersionAttribute}.g.cs" },
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
#if NET8_0_OR_GREATER
        var lines = NewLineRegex().Split(source);
#else
        var lines = Regex.Split(source, "\r\n");
#endif
        var linesWithIndentation = string.Join("\r\n", lines.Select(func));
        return linesWithIndentation;
    }

    /// <summary>
    /// Clones every method the test synchronizes, changing nothing but its name, and fails the
    /// test when a clone does not compile. Such a failure is in the cloning layer, whatever the
    /// test itself is about.
    /// </summary>
    private static void EnsureIdentityClonesCompile(CSharpCompilation compilation, CSharpParseOptions parseOptions)
    {
        // The sync generator runs as well, since it declares the attributes the source uses.
        var result = CSharpGeneratorDriver.Create(new SyncMethodSourceGenerator(), new IdentityCloneGenerator())
            .WithUpdatedParseOptions(parseOptions)
            .RunGenerators(compilation)
            .GetRunResult();

        var identity = result.Results.Single(r => r.Generator.GetGeneratorType() == typeof(IdentityCloneGenerator));

        if (identity.Exception is { } exception)
        {
            throw new InvalidOperationException("Identity clone threw", exception);
        }

        // Every method which is synchronized is cloned as well, so that a pass which cloned
        // nothing cannot pass for one whose clones compile.
        var synchronized = result.Results
            .Single(r => r.Generator.GetGeneratorType() == typeof(SyncMethodSourceGenerator))
            .GeneratedSources
            .Count(s => s.HintName is not ($"{SyncMethodSourceGenerator.CreateSyncVersionAttribute}.g.cs" or $"{SyncMethodSourceGenerator.SkipSyncVersionAttribute}.g.cs"));

        if (identity.GeneratedSources.Length < synchronized)
        {
            throw new InvalidOperationException(
                $"Identity clone produced {identity.GeneratedSources.Length} files for {synchronized} synchronized methods");
        }

        var errors = identity.Diagnostics
            .Concat(compilation.AddSyntaxTrees(result.GeneratedTrees).GetDiagnostics())
            .Where(d => d.Severity == DiagnosticSeverity.Error)
            .ToArray();

        if (errors.Length > 0)
        {
            throw new InvalidOperationException(
                "Identity clone does not compile:\n" + string.Join("\n", errors.Select(d => d.ToString())));
        }
    }

#if NET8_0_OR_GREATER
    [GeneratedRegex("\r\n")]
    private static partial Regex NewLineRegex();
#endif
}
