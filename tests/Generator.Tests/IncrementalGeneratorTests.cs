using System.Reflection;
using Zomp.SyncMethodGenerator;

namespace Generator.Tests;

public class IncrementalGeneratorTests
{
    [Theory]
    [InlineData(
        IncrementalStepRunReason.Cached,
        IncrementalStepRunReason.Unchanged,
        IncrementalStepRunReason.Cached,
        """
        using System;
        using System.Threading.Tasks;

        class Test
        {
            public void ProgressMethod() { }
            public Task ProgressMethodAsync() => Task.CompletedTask;
        
            [Zomp.SyncMethodGenerator.CreateSyncVersion]
            public async Task CallProgressMethodAsync()
            {
                await ProgressMethodAsync();
            }
        }
        """,
        """
        using System;
        using System.Threading.Tasks;

        class Test
        {
            public void ProgressMethod() { }
            public Task ProgressMethodAsync() => Task.Yield();
        
            [Zomp.SyncMethodGenerator.CreateSyncVersion]
            public async Task CallProgressMethodAsync()
            {
                await ProgressMethodAsync();
            }
        }
        """)]
    [InlineData(
        IncrementalStepRunReason.Modified,
        IncrementalStepRunReason.Modified,
        IncrementalStepRunReason.Modified,
        """
        using System;
        using System.Threading.Tasks;
        
        class Test
        {
            public void ProgressMethod() { }
            public Task ProgressMethodAsync() => Task.CompletedTask;
        
            [Zomp.SyncMethodGenerator.CreateSyncVersion]
            public async Task CallProgressMethodAsync()
            {
            }
        }
        """,
        """
        using System;
        using System.Threading.Tasks;
        
        class Test
        {
            public void ProgressMethod() { }
            public Task ProgressMethodAsync() => Task.CompletedTask;
        
            [Zomp.SyncMethodGenerator.CreateSyncVersion]
            public async Task CallProgressMethodAsync()
            {
                await ProgressMethodAsync();
            }
        }
        """)]
    public void CheckGeneratorIsIncremental(
        IncrementalStepRunReason sourceStepReason,
        IncrementalStepRunReason executeStepReason,
        IncrementalStepRunReason combineStepReason,
        string source,
        string sourceUpdated)
    {
        SyntaxTree baseSyntaxTree = CSharpSyntaxTree.ParseText(source);

        Compilation compilation = CSharpCompilation.Create(
            "compilation",
            new[] { baseSyntaxTree },
            new[] { MetadataReference.CreateFromFile(typeof(Binder).GetTypeInfo().Assembly.Location) },
            new CSharpCompilationOptions(OutputKind.ConsoleApplication));

        ISourceGenerator sourceGenerator = new SyncMethodSourceGenerator().AsSourceGenerator();

        GeneratorDriver driver = CSharpGeneratorDriver.Create(
            generators: new[] { sourceGenerator },
            driverOptions: new GeneratorDriverOptions(default, trackIncrementalGeneratorSteps: true));

        // Run the generator
        driver = driver.RunGenerators(compilation);

        // Update the compilation and rerun the generator
        compilation = compilation.ReplaceSyntaxTree(baseSyntaxTree, CSharpSyntaxTree.ParseText(sourceUpdated));
        driver = driver.RunGenerators(compilation);

        GeneratorRunResult result = driver.GetRunResult().Results.Single();
        IEnumerable<(object Value, IncrementalStepRunReason Reason)> sourceOutputs =
            result.TrackedOutputSteps.SelectMany(outputStep => outputStep.Value).SelectMany(output => output.Outputs);
        var (value, reason) = Assert.Single(sourceOutputs);
        Assert.Equal(sourceStepReason, reason);
        Assert.Equal(executeStepReason, result.TrackedSteps["GetMethodToGenerate"].Single().Outputs[0].Reason);
        Assert.Equal(combineStepReason, result.TrackedSteps["GenerateMethodSource"].Single().Outputs[0].Reason);
    }
}
