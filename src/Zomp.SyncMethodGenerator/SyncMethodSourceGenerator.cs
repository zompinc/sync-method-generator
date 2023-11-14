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
    internal const string QualifiedCreateSyncVersionAttribute = $"{ThisAssembly.RootNamespace}.{CreateSyncVersionAttribute}";

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

        IncrementalValuesProvider<MethodDeclarationSyntax> methodDeclarations = context.SyntaxProvider
            .ForAttributeWithMetadataName(
                QualifiedCreateSyncVersionAttribute,
                predicate: static (s, _) => IsSyntaxTargetForGeneration(s),
                transform: static (ctx, _) => (MethodDeclarationSyntax)ctx.TargetNode);

        IncrementalValueProvider<(Compilation, ImmutableArray<MethodDeclarationSyntax>)> compilationAndMethods
            = context.CompilationProvider.Combine(methodDeclarations.Collect());

        context.RegisterSourceOutput(
            compilationAndMethods,
            static (spc, source) => Execute(source.Item1, source.Item2, spc));
    }

    private static bool IsSyntaxTargetForGeneration(SyntaxNode node)
        => node is MethodDeclarationSyntax m && m.AttributeLists.Count > 0;

    private static void Execute(Compilation compilation, ImmutableArray<MethodDeclarationSyntax> methods, SourceProductionContext context)
    {
        if (methods.IsDefaultOrEmpty)
        {
            // nothing to do yet
            return;
        }

        // I'm not sure if this is actually necessary, but `[LoggerMessage]` does it, so seems like a good idea!
        IEnumerable<MethodDeclarationSyntax> distinctMethods = methods.Distinct();

        // Convert each MethodDeclarationSyntax to an MethodToGenerate
        List<MethodToGenerate> methodsToGenerate = GetTypesToGenerate(context, compilation, distinctMethods, context.CancellationToken);

        // If there were errors in the MethodDeclarationSyntax, we won't create an
        // MethodToGenerate for it, so make sure we have something to generate
        if (methodsToGenerate.Count > 0)
        {
            // Generate the source code and add it to the output
            var sourceDictionary = new Dictionary<string, string>();
            foreach (var m in methodsToGenerate)
            {
                // Ensure there are no collisions in generated names
                var i = 1;
                while (true)
                {
                    var sourcePath = $"{string.Join(".", m.Namespaces)}" +
                        $".{string.Join(".", m.Classes.Select(c => c.ClassName))}" +
                        $".{m.MethodName + (i == 1 ? string.Empty : "_" + i)}.g.cs";

                    if (!sourceDictionary.ContainsKey(sourcePath))
                    {
                        var source = SourceGenerationHelper.GenerateExtensionClass(m);
                        sourceDictionary.Add(sourcePath, source);
                        break;
                    }

                    ++i;
                }
            }

            foreach (var entry in sourceDictionary)
            {
                context.AddSource(entry.Key, SourceText.From(entry.Value, Encoding.UTF8));
            }
        }
    }

    private static List<MethodToGenerate> GetTypesToGenerate(SourceProductionContext context, Compilation compilation, IEnumerable<MethodDeclarationSyntax> methodDeclarations, CancellationToken ct)
    {
        var methodsToGenerate = new List<MethodToGenerate>();
        var replacementOverrides = new Dictionary<string, string?>();

        INamedTypeSymbol? attribute = compilation.GetTypeByMetadataName(QualifiedCreateSyncVersionAttribute);
        if (attribute == null)
        {
            // nothing to do if this type isn't available
            return methodsToGenerate;
        }

        foreach (var methodDeclarationSyntax in methodDeclarations)
        {
            // stop if we're asked to
            ct.ThrowIfCancellationRequested();

            SemanticModel semanticModel = compilation.GetSemanticModel(methodDeclarationSyntax.SyntaxTree);

            if (semanticModel.GetDeclaredSymbol(methodDeclarationSyntax, cancellationToken: ct) is not IMethodSymbol methodSymbol)
            {
                // something went wrong
                continue;
            }

            if (!methodSymbol.IsAsync && !AsyncToSyncRewriter.IsTypeOfInterest(methodSymbol.ReturnType))
            {
                continue;
            }

            var methodName = methodSymbol.ToString();

            var variations = 0;
            foreach (AttributeData attributeData in methodSymbol.GetAttributes())
            {
                if (!attribute.Equals(attributeData.AttributeClass, SymbolEqualityComparer.Default))
                {
                    continue;
                }

                if (attributeData.NamedArguments[0].Value.Value is int value)
                {
                    variations = value;
                }

                break;
            }

            var classes = new List<ClassDeclaration>();
            SyntaxNode? node = methodDeclarationSyntax;
            while (node.Parent is not null)
            {
                node = node.Parent;
                if (node is not ClassDeclarationSyntax classSyntax)
                {
                    break;
                }

                var modifiers = new List<SyntaxKind>();

                foreach (var mod in classSyntax.Modifiers)
                {
                    var kind = mod.RawKind;
                    if (kind == (int)SyntaxKind.PartialKeyword)
                    {
                        continue;
                    }

                    modifiers.Add((SyntaxKind)kind);
                }

                classes.Insert(0, new(classSyntax.Identifier.ValueText, modifiers, classSyntax.TypeParameterList));
            }

            if (classes.Count == 0)
            {
                continue;
            }

            var @collections = (CollectionTypes)variations;

            //fill out replacement overrides dictionary
            var rewriter = new AsyncToSyncRewriter(semanticModel, replacementOverrides);
            var sn = rewriter.Visit(methodDeclarationSyntax);
            var content = sn.ToFullString();

            var diagnostics = rewriter.Diagnostics;

            var hasErrors = false;
            foreach (var diagnostic in diagnostics)
            {
                context.ReportDiagnostic(diagnostic);
                hasErrors |= diagnostic.Severity == DiagnosticSeverity.Error;
            }

            if (hasErrors)
            {
                continue;
            }

            var isNamespaceFileScoped = false;
            var namespaces = new List<string>();
            while (node is not null && node is not CompilationUnitSyntax)
            {
                switch (node)
                {
                    case NamespaceDeclarationSyntax nds:
                        namespaces.Insert(0, nds.Name.ToString());
                        break;
                    case FileScopedNamespaceDeclarationSyntax file:
                        namespaces.Add(file.Name.ToString());
                        isNamespaceFileScoped = true;
                        break;
                    default:
                        throw new InvalidOperationException($"Cannot handle {node}");
                }

                node = node.Parent;
            }

            methodsToGenerate.Add(new(namespaces, isNamespaceFileScoped, classes, methodDeclarationSyntax.Identifier.ValueText, content));
        }

        return methodsToGenerate;
    }
}
