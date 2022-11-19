namespace Zomp.SyncMethodGenerator;

/// <summary>
/// Generates synchronous code from asynchronous
/// </summary>
[Generator]
public class SyncMethodSourceGenerator : IIncrementalGenerator
{
    internal const string CreateSyncVersionAttribute = "Zomp.SyncMethodGenerator.CreateSyncVersionAttribute";

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
            "CreateSyncVersionAttribute.g.cs", SourceText.From(SourceGenerationHelper.Attribute, Encoding.UTF8)));

        IncrementalValuesProvider<MethodDeclarationSyntax> methodDeclarations = context.SyntaxProvider
            .CreateSyntaxProvider(
                predicate: static (s, _) => IsSyntaxTargetForGeneration(s),
                transform: static (ctx, _) => GetSemanticTargetForGeneration(ctx))
            .Where(static m => m is not null)!;

        IncrementalValueProvider<(Compilation, ImmutableArray<MethodDeclarationSyntax>)> compilationAndMethods
            = context.CompilationProvider.Combine(methodDeclarations.Collect());

        context.RegisterSourceOutput(compilationAndMethods,
            static (spc, source) => Execute(source.Item1, source.Item2, spc));
    }

    static bool IsSyntaxTargetForGeneration(SyntaxNode node)
        => node is MethodDeclarationSyntax m && m.AttributeLists.Count > 0;

    static MethodDeclarationSyntax? GetSemanticTargetForGeneration(GeneratorSyntaxContext context)
    {
        // we know the node is a MethodDeclarationSyntax thanks to IsSyntaxTargetForGeneration
        var MethodDeclarationSyntax = (MethodDeclarationSyntax)context.Node;

        // loop through all the attributes on the method
        foreach (var attributeListSyntax in MethodDeclarationSyntax.AttributeLists)
        {
            foreach (var attributeSyntax in attributeListSyntax.Attributes)
            {
                if (context.SemanticModel.GetSymbolInfo(attributeSyntax).Symbol is not IMethodSymbol attributeSymbol)
                {
                    // weird, we couldn't get the symbol, ignore it
                    continue;
                }

                var attributeContainingTypeSymbol = attributeSymbol.ContainingType;
                var fullName = attributeContainingTypeSymbol.ToDisplayString();

                // Is the attribute the [CreateSyncVersion] attribute?
                if (fullName == CreateSyncVersionAttribute)
                {
                    // return the method
                    return MethodDeclarationSyntax;
                }
            }
        }

        // we didn't find the attribute we were looking for
        return null;
    }

    static void Execute(Compilation compilation, ImmutableArray<MethodDeclarationSyntax> methods, SourceProductionContext context)
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
                for (; ; )
                {
                    var sourcePath = $"{string.Join(".", m.Namespaces)}" +
                        $".{string.Join(".", m.Classes.Select(c => c.ClassName))}" +
                        $".{m.MethodName + (i == 1 ? string.Empty : "_" + i)}.g.cs";

                    if (!sourceDictionary.ContainsKey(sourcePath))
                    {
                        sourceDictionary.Add(sourcePath, SourceGenerationHelper.GenerateExtensionClass(m));
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

    static List<MethodToGenerate> GetTypesToGenerate(SourceProductionContext context, Compilation compilation, IEnumerable<MethodDeclarationSyntax> methodDeclarations, CancellationToken ct)
    {
        var methodsToGenerate = new List<MethodToGenerate>();
        INamedTypeSymbol? attribute = compilation.GetTypeByMetadataName(CreateSyncVersionAttribute);
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

            string methodName = methodSymbol.ToString();

            foreach (AttributeData attributeData in methodSymbol.GetAttributes())
            {
                if (!attribute.Equals(attributeData.AttributeClass, SymbolEqualityComparer.Default))
                {
                    continue;
                }
                break;
            }

            var classes = new List<Class>();
            SyntaxNode? node = methodDeclarationSyntax;
            ClassDeclarationSyntax? partialMissing = null;
            while (node.Parent is not null)
            {
                node = node.Parent;
                if (node is not ClassDeclarationSyntax classSyntax)
                {
                    break;
                }

                if (!classSyntax.Modifiers.Any(SyntaxKind.PartialKeyword))
                {
                    partialMissing = classSyntax;
                    break;
                }

                var modifiers = new List<SyntaxKind>();

                foreach (var mod in classSyntax.Modifiers)
                {
                    var kind = mod.RawKind;
                    if (kind == (int)SyntaxKind.PartialKeyword)
                        continue;
                    modifiers.Add((SyntaxKind)kind);
                }
                classes.Insert(0, new(classSyntax.Identifier.ValueText, modifiers));
            }

            if (partialMissing is not null)
            {
                context.ReportDiagnostic(Diagnostic.Create(ClassMustBePartial, partialMissing.GetLocation(), partialMissing.Identifier.ValueText));
                continue;
            }

            if (classes.Count == 0)
            {
                continue;
            }

            string content;
            try
            {
                var rewriter = new AsyncToSyncRewriter(semanticModel);
                var sn = rewriter.Visit(methodDeclarationSyntax);
                content = sn.ToFullString();

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
            }
            catch (Exception ex)
            {
                context.ReportDiagnostic(Diagnostic.Create(ExceptionOccured, location: null, ex.ToString()));
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
                    case FileScopedNamespaceDeclarationSyntax fsnds:
                        namespaces.Add(fsnds.Name.ToString());
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
