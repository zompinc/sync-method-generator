using Basic.Reference.Assemblies;
using Zomp.SyncMethodGenerator;

namespace Generator.Tests;

/// <summary>
/// Verifies generation when the marker attribute is compiled into more than one referenced assembly.
/// This happens in a chain of projects which use <c>InternalsVisibleTo</c> together with
/// <c>SYNC_METHOD_GENERATOR_DISABLE_ATTRIBUTE_GENERATION</c>.
/// </summary>
public class MultipleAttributeDefinitionsTests
{
    private const string DisableAttributeGeneration = "SYNC_METHOD_GENERATOR_DISABLE_ATTRIBUTE_GENERATION";

    [Fact]
    public void GenerateWhenAttributeIsDefinedInMultipleReferences()
    {
        var (compilation, result) = RunGenerator("""
using System.Threading.Tasks;

namespace Consuming;

public partial class Class
{
    [Zomp.SyncMethodGenerator.CreateSyncVersion]
    public async Task MethodAsync() => await Task.CompletedTask;
}
""");

        var generated = Assert.Single(GeneratedFor(result, "MethodAsync"));
        Assert.Contains("public void Method()", generated.ToString(), StringComparison.Ordinal);

        var errors = compilation.AddSyntaxTrees(result.GeneratedTrees)
            .GetDiagnostics()
            .Where(d => d.Severity == DiagnosticSeverity.Error);

        Assert.Empty(errors);
    }

    [Fact]
    public void SkipAttributeIsHonoredWhenAttributesAreDefinedInMultipleReferences()
    {
        var (_, result) = RunGenerator("""
using System.Threading.Tasks;

namespace Consuming;

[Zomp.SyncMethodGenerator.CreateSyncVersion]
public partial class Class
{
    public async Task IncludedAsync() => await Task.CompletedTask;

    [Zomp.SyncMethodGenerator.SkipSyncVersion]
    public async Task ExcludedAsync() => await Task.CompletedTask;
}
""");

        _ = Assert.Single(GeneratedFor(result, "IncludedAsync"));
        Assert.Empty(GeneratedFor(result, "ExcludedAsync"));
    }

    private static IEnumerable<SyntaxTree> GeneratedFor(GeneratorDriverRunResult result, string methodName)
        => result.GeneratedTrees.Where(t => t.FilePath.EndsWith($"Consuming.Class.{methodName}.g.cs", StringComparison.Ordinal));

    private static (CSharpCompilation Compilation, GeneratorDriverRunResult Result) RunGenerator(string source)
    {
        // Each library bakes in its own internal copy of the marker attributes. Only the second one
        // exposes its internals to the consumer, so attribute usages still bind uniquely.
        var first = CreateLibraryWithAttribute("First", Net100.References.All, "Second");
        var second = CreateLibraryWithAttribute("Second", [.. Net100.References.All, first], "Consumer");

        var parseOptions = CSharpParseOptions.Default
            .WithLanguageVersion(LanguageVersion.Preview)
            .WithPreprocessorSymbols(DisableAttributeGeneration);

        var compilation = CSharpCompilation.Create(
            assemblyName: "Consumer",
            syntaxTrees: [CSharpSyntaxTree.ParseText(source, parseOptions)],
            references: [.. Net100.References.All, first, second],
            options: new(OutputKind.DynamicallyLinkedLibrary));

        var result = CSharpGeneratorDriver
            .Create(new SyncMethodSourceGenerator())
            .WithUpdatedParseOptions(parseOptions)
            .RunGenerators(compilation)
            .GetRunResult();

        return (compilation, result);
    }

    private static PortableExecutableReference CreateLibraryWithAttribute(
        string assemblyName,
        IEnumerable<MetadataReference> references,
        params string[] internalsVisibleTo)
    {
        var source = string.Join(
            Environment.NewLine,
            internalsVisibleTo.Select(a => $"""[assembly: System.Runtime.CompilerServices.InternalsVisibleTo("{a}")]"""));

        var compilation = CSharpCompilation.Create(
            assemblyName,
            [CSharpSyntaxTree.ParseText(source)],
            references,
            new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        // The generator contributes its own internal copy of the marker attribute to this assembly.
        _ = CSharpGeneratorDriver
            .Create(new SyncMethodSourceGenerator())
            .RunGeneratorsAndUpdateCompilation(compilation, out var withAttribute, out _);

        using var peStream = new MemoryStream();
        var emitResult = withAttribute.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        peStream.Position = 0;
        return MetadataReference.CreateFromStream(peStream);
    }
}
