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

    internal const string OmitNullableDirective = "OmitNullableDirective";
    internal const string Name = "Name";

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

        var disableNullable =
            context.CompilationProvider.Select((c, _)
                => c is CSharpCompilation { LanguageVersion: < LanguageVersion.CSharp8 });

        var methodSourceTexts = context.SyntaxProvider
            .ForAttributeWithMetadataName(
                QualifiedCreateSyncVersionAttribute,
                predicate: static (s, _) => s is MethodDeclarationSyntax { AttributeLists.Count: > 0 },
                transform: static (ctx, _) => ctx)
            .Combine(disableNullable)
            .Select((data, ct) => GetMethodToGenerate(data.Left, (MethodDeclarationSyntax)data.Left.TargetNode, data.Right, ct)!)
            .WithTrackingName("GetMethodToGenerate")
            .Where(static s => s is not null)
            .Select(static (m, _) => GenerateSource(m))
            .WithTrackingName("GenerateMethodSource");

        AddSourceTexts(context, methodSourceTexts);

        var classSourceTexts = context.SyntaxProvider
            .ForAttributeWithMetadataName(
                QualifiedCreateSyncVersionAttribute,
                predicate: static (s, _) => s is TypeDeclarationSyntax { AttributeLists.Count: > 0 },
                transform: static (ctx, _) => ctx)
            .Combine(disableNullable)
            .SelectMany((data, ct) => GetClassToGenerate(data.Left, (TypeDeclarationSyntax)data.Left.TargetNode, data.Right, ct)!)
            .WithTrackingName("GetClassToGenerate")
            .Select(static (m, _) => GenerateSource(m))
            .WithTrackingName("GenerateClassSource");

        AddSourceTexts(context, classSourceTexts);
    }

    private static void AddSourceTexts(IncrementalGeneratorInitializationContext context, IncrementalValuesProvider<(MethodToGenerate MethodToGenerate, string Path, string Content)> classSourceTexts)
    {
        context.RegisterSourceOutput(
            classSourceTexts,
            static (spc, source) =>
            {
                foreach (var diagnostic in source.MethodToGenerate.Diagnostics)
                {
                    spc.ReportDiagnostic(diagnostic);
                }

                if (!source.MethodToGenerate.HasErrors)
                {
                    spc.AddSource(source.Path, SourceText.From(source.Content, Encoding.UTF8));
                }
            });
    }

    private static (MethodToGenerate MethodToGenerate, string Path, string Content) GenerateSource(MethodToGenerate m)
    {
        static string BuildClassName(ClassDeclaration c)
        {
            if (c.TypeParameterListSyntax.IsEmpty)
            {
                return c.ClassName;
            }

            return c.ClassName + "{" + string.Join(",", c.TypeParameterListSyntax) + "}";
        }

        var sourcePath = $"{string.Join(".", m.Namespaces)}" +
            $".{string.Join(".", m.Classes.Select(BuildClassName))}" +
            $".{m.MethodName + (m.Index == 1 ? string.Empty : "_" + m.Index)}.g.cs";

        var source = SourceGenerationHelper.GenerateExtensionClass(m);

        return (m, sourcePath, source);
    }

    private static ImmutableArray<MethodToGenerate> GetClassToGenerate(GeneratorAttributeSyntaxContext context, TypeDeclarationSyntax typeDeclartionSyntax, bool disableNullable, CancellationToken ct)
    {
        if (context.TargetSymbol is not ITypeSymbol typeSymbol)
        {
            // the attribute isn't on a method
            return default;
        }

        INamedTypeSymbol? attribute = context.SemanticModel.Compilation.GetTypeByMetadataName(QualifiedCreateSyncVersionAttribute);
        if (attribute == null)
        {
            // nothing to do if this type isn't available
            return default;
        }

        AttributeData syncMethodGeneratorAttributeData = null!;

        foreach (AttributeData attributeData in typeSymbol.GetAttributes())
        {
            if (!attribute.Equals(attributeData.AttributeClass, SymbolEqualityComparer.Default))
            {
                continue;
            }

            syncMethodGeneratorAttributeData = attributeData;
            break;
        }

        var className = syncMethodGeneratorAttributeData.NamedArguments.FirstOrDefault(c => c.Key == Name).Value.Value as string;
        var array = ImmutableArray.CreateBuilder<MethodToGenerate>();

        foreach (var member in typeDeclartionSyntax.Members)
        {
            if (member is not MethodDeclarationSyntax method)
            {
                continue;
            }

            var m = GetMethodToGenerate(context, method, disableNullable, ct, attributeOptional: true);

            if (m is null)
            {
                continue;
            }

            if (className != null)
            {
                m = m with
                {
                    Classes = ImmutableArray.Create(CreateClassDeclaration(typeDeclartionSyntax, className)),
                };
            }

            array.Add(m);
        }

        return array.ToImmutable();
    }

    private static MethodToGenerate? GetMethodToGenerate(GeneratorAttributeSyntaxContext context, MethodDeclarationSyntax methodDeclarationSyntax, bool disableNullable, CancellationToken ct, bool attributeOptional = false)
    {
        // stop if we're asked to
        ct.ThrowIfCancellationRequested();

        if (context.TargetSymbol is not IMethodSymbol methodSymbol)
        {
            var symbolFromModel = context.SemanticModel.GetDeclaredSymbol(methodDeclarationSyntax);

            if (symbolFromModel is null)
            {
                // the attribute isn't on a method
                return null;
            }

            methodSymbol = symbolFromModel;
        }

        INamedTypeSymbol? attribute = context.SemanticModel.Compilation.GetTypeByMetadataName(QualifiedCreateSyncVersionAttribute);
        if (attribute == null)
        {
            // nothing to do if this type isn't available
            return null;
        }

        // find the index of the method in the containing type
        var index = 1;

        if (methodSymbol.ContainingType is { } containingType)
        {
            foreach (var member in containingType.GetMembers())
            {
                if (member.Equals(methodSymbol, SymbolEqualityComparer.Default))
                {
                    break;
                }

                if (member.Name.Equals(methodSymbol.Name, StringComparison.Ordinal))
                {
                    ++index;
                }
            }
        }

        if (!methodSymbol.IsAsync && !AsyncToSyncRewriter.IsTypeOfInterest(methodSymbol.ReturnType))
        {
            return null;
        }

        string? name = null;
        var explicitDisableNullable = false;

        AttributeData? syncMethodGeneratorAttributeData = null;

        foreach (AttributeData attributeData in methodSymbol.GetAttributes())
        {
            if (!attribute.Equals(attributeData.AttributeClass, SymbolEqualityComparer.Default))
            {
                continue;
            }

            syncMethodGeneratorAttributeData = attributeData;
            break;
        }

        if (syncMethodGeneratorAttributeData != null)
        {
            explicitDisableNullable = syncMethodGeneratorAttributeData.NamedArguments.FirstOrDefault(c => c.Key == OmitNullableDirective) is { Value.Value: true };
            name = syncMethodGeneratorAttributeData.NamedArguments.FirstOrDefault(c => c.Key == Name).Value.Value as string;
        }

        disableNullable |= explicitDisableNullable;

        var classes = ImmutableArray.CreateBuilder<ClassDeclaration>();
        SyntaxNode? node = methodDeclarationSyntax;
        while (node.Parent is not null)
        {
            node = node.Parent;
            if (node is not ClassDeclarationSyntax classSyntax)
            {
                break;
            }

            classes.Insert(0, CreateClassDeclaration(classSyntax));
        }

        if (classes.Count == 0)
        {
            return null;
        }

        var rewriter = new AsyncToSyncRewriter(context.SemanticModel);
        var sn = rewriter.Visit(methodDeclarationSyntax);
        var content = sn.ToFullString();

        var diagnostics = rewriter.Diagnostics;

        var hasErrors = false;
        foreach (var diagnostic in diagnostics)
        {
            hasErrors |= diagnostic.Descriptor.DefaultSeverity == DiagnosticSeverity.Error;
        }

        var isNamespaceFileScoped = false;
        var namespaces = ImmutableArray.CreateBuilder<string>();

        if (!hasErrors)
        {
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
        }

        return new MethodToGenerate(index, namespaces.ToImmutable(), isNamespaceFileScoped, classes.ToImmutable(), name ?? methodDeclarationSyntax.Identifier.ValueText, content, disableNullable, rewriter.Diagnostics, hasErrors);
    }

    private static ClassDeclaration CreateClassDeclaration(TypeDeclarationSyntax classSyntax, string? name = null)
    {
        var modifiers = ImmutableArray.CreateBuilder<ushort>();

        foreach (var mod in classSyntax.Modifiers)
        {
            var kind = mod.RawKind;
            if (kind == (int)SyntaxKind.PartialKeyword)
            {
                continue;
            }

            modifiers.Add((ushort)kind);
        }

        var typeParameters = ImmutableArray.CreateBuilder<string>();

        foreach (var typeParameter in classSyntax.TypeParameterList?.Parameters ?? default)
        {
            typeParameters.Add(typeParameter.Identifier.ValueText);
        }

        return new ClassDeclaration(name ?? classSyntax.Identifier.ValueText, modifiers.ToImmutable(), typeParameters.ToImmutable());
    }
}
