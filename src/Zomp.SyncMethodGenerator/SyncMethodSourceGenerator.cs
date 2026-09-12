namespace Zomp.SyncMethodGenerator;

/// <summary>
/// Generates synchronous code from asynchronous.
/// </summary>
[Generator]
public class SyncMethodSourceGenerator : IIncrementalGenerator
{
    /// <summary>
    /// Create sync version attribute string.
    /// </summary>
    public const string CreateSyncVersionAttribute = "CreateSyncVersionAttribute";

    /// <summary>
    /// Skip sync version attribute string.
    /// </summary>
    public const string SkipSyncVersionAttribute = "SkipSyncVersionAttribute";

    internal const string QualifiedCreateSyncVersionAttribute = $"{ThisAssembly.RootNamespace}.{CreateSyncVersionAttribute}";
    internal const string QualifiedSkipSyncVersionAttribute = $"{ThisAssembly.RootNamespace}.{SkipSyncVersionAttribute}";

    internal const string OmitNullableDirective = "OmitNullableDirective";
    internal const string PreserveProgress = "PreserveProgress";
    internal const string PreserveCancellationToken = "PreserveCancellationToken";

    /// <inheritdoc/>
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        // To start debugger compile with /p:DefineConstants=DEBUG_SMG
#if DEBUG_SMG
        if (!Debugger.IsAttached)
        {
            Debugger.Launch();
        }
#endif
        context.RegisterPostInitializationOutput(ctx => ctx.AddSource(
            $"{CreateSyncVersionAttribute}.g.cs", SourceText.From(SourceGenerationHelper.CreateSyncVersionAttributeSource, Encoding.UTF8)));

        context.RegisterPostInitializationOutput(ctx => ctx.AddSource(
            $"{SkipSyncVersionAttribute}.g.cs", SourceText.From(SourceGenerationHelper.SkipSyncVersionAttributeSource, Encoding.UTF8)));

        var disableNullable =
            context.CompilationProvider.Select((c, _) =>
            {
                var isNullableDisabledInProject = c.Options.NullableContextOptions == NullableContextOptions.Disable;
                var isLanguageVersionBelowCSharp8 = c is CSharpCompilation { LanguageVersion: < LanguageVersion.CSharp8 };
                return isNullableDisabledInProject || isLanguageVersionBelowCSharp8;
            });

        var methodDeclarations = CloneTarget.ForAttribute(context.SyntaxProvider, QualifiedCreateSyncVersionAttribute)
            .Combine(disableNullable)
            .Select((data, ct) => GetMethodToGenerate(data.Left.Context, data.Left.Syntax, data.Right, ct)!)
            .WithTrackingName("GetMethodToGenerate")
            .Where(static s => s is not null);

        ClonedMethodOutput.Register(context, methodDeclarations, CollidingOverloads);
    }

    private static ClonedMethod? GetMethodToGenerate(GeneratorAttributeSyntaxContext context, MethodDeclarationSyntax methodDeclarationSyntax, bool disableNullable, CancellationToken ct)
    {
        // stop if we're asked to
        ct.ThrowIfCancellationRequested();

        var isTargetTypeSymbol = context.TargetSymbol is ITypeSymbol;

        var methodSymbol = isTargetTypeSymbol
            ? context.SemanticModel.GetDeclaredSymbol(methodDeclarationSyntax, ct)
            : context.TargetSymbol as IMethodSymbol;

        if (methodSymbol == null)
        {
            return null;
        }

        if (!methodSymbol.IsAsync
            && (methodSymbol.ReturnType is not INamedTypeSymbol named
                || !AsyncToSyncRewriter.IsTypeOfInterest(named)))
        {
            return null;
        }

        foreach (var attributeData in methodSymbol.GetAttributes())
        {
            var attributeClassName = attributeData.AttributeClass?.ToDisplayString();

            if (attributeClassName == QualifiedSkipSyncVersionAttribute)
            {
                // Skip processing if the method has the skip attribute applied
                return null;
            }

            if (isTargetTypeSymbol && attributeClassName == QualifiedCreateSyncVersionAttribute)
            {
                // Skip processing if the attribute is defined on the type to prioritize method-level attribute
                return null;
            }
        }

        if (!MethodLocation.TryCreate(methodDeclarationSyntax, methodSymbol, out var location, out var root))
        {
            return null;
        }

        var syncMethodGeneratorAttributeData = context.Attributes[0];

        var explicitDisableNullable = syncMethodGeneratorAttributeData.NamedArguments.FirstOrDefault(c => c.Key == OmitNullableDirective) is { Value.Value: true };
        disableNullable |= explicitDisableNullable;

        var preserveProgress = syncMethodGeneratorAttributeData.NamedArguments.FirstOrDefault(c => c.Key == PreserveProgress) is { Value.Value: true };
        var preserveCancellationToken = syncMethodGeneratorAttributeData.NamedArguments.FirstOrDefault(c => c.Key == PreserveCancellationToken) is { Value.Value: true };

        var rewriter = new AsyncToSyncRewriter(context.SemanticModel, disableNullable, preserveProgress, preserveCancellationToken, methodDeclarationSyntax);
        var rewritten = rewriter.Visit(root);

        return ClonedMethod.Create(location, methodDeclarationSyntax, rewritten, disableNullable, rewriter.Diagnostics);
    }
}
