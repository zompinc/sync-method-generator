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
    /// Ignore create sync version attribute string.
    /// </summary>
    public const string IgnoreCreateSyncVersionAttribute = "IgnoreCreateSyncVersionAttribute";

    internal const string QualifiedCreateSyncVersionAttribute = $"{ThisAssembly.RootNamespace}.{CreateSyncVersionAttribute}";
    internal const string QualifiedIgnoreCreateSyncVersionAttribute = $"{ThisAssembly.RootNamespace}.{IgnoreCreateSyncVersionAttribute}";

    internal const string OmitNullableDirective = "OmitNullableDirective";
    internal const string PreserveProgress = "PreserveProgress";

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
            $"{IgnoreCreateSyncVersionAttribute}.g.cs", SourceText.From(SourceGenerationHelper.IgnoreCreateSyncVersionAttributeSource, Encoding.UTF8)));

        var disableNullable =
            context.CompilationProvider.Select((c, _) =>
            {
                var isNullableDisabledInProject = c.Options.NullableContextOptions == NullableContextOptions.Disable;
                var isLanguageVersionBelowCSharp8 = c is CSharpCompilation { LanguageVersion: < LanguageVersion.CSharp8 };
                return isNullableDisabledInProject || isLanguageVersionBelowCSharp8;
            });

        var methodDeclarations = context.SyntaxProvider
            .ForAttributeWithMetadataName(
                QualifiedCreateSyncVersionAttribute,
                predicate: static (s, _) => IsSyntaxTargetForGeneration(s),
                transform: static (ctx, ct) => TransformForGeneration(ctx, ct))
            .SelectMany((list, ct) => list)
            .Combine(disableNullable)
            .Select((data, ct) => GetMethodToGenerate(data.Left.Context, data.Left.Syntax, data.Right, ct)!)
            .WithTrackingName("GetMethodToGenerate")
            .Where(static s => s is not null);

        var sourceTexts = methodDeclarations
            .Select(static (m, _) => GenerateSource(m))
            .WithTrackingName("GenerateSource");

        context.RegisterSourceOutput(
            sourceTexts,
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

    private static bool IsSyntaxTargetForGeneration(SyntaxNode node) => node switch
    {
        MethodDeclarationSyntax m when m.AttributeLists.Count > 0 => true,
        ClassDeclarationSyntax c when c.AttributeLists.Count > 0 => true,
        StructDeclarationSyntax s when s.AttributeLists.Count > 0 => true,
        InterfaceDeclarationSyntax i when i.AttributeLists.Count > 0 => true,
        _ => false,
    };

    private static ImmutableArray<TransformResult> TransformForGeneration(GeneratorAttributeSyntaxContext ctx, CancellationToken ct)
    {
        ct.ThrowIfCancellationRequested();

        if (ctx.TargetNode is TypeDeclarationSyntax typeDecl)
        {
            return [.. typeDecl.Members.OfType<MethodDeclarationSyntax>().Select(s => new TransformResult(ctx, s))];
        }
        else if (ctx.TargetNode is MethodDeclarationSyntax methodDecl)
        {
            return [new TransformResult(ctx, methodDecl)];
        }

        return [];
    }

    private static MethodToGenerate? GetMethodToGenerate(GeneratorAttributeSyntaxContext context, MethodDeclarationSyntax methodDeclarationSyntax, bool disableNullable, CancellationToken ct)
    {
        // stop if we're asked to
        ct.ThrowIfCancellationRequested();

        var isTargetTypeSymbol = context.TargetSymbol is ITypeSymbol;

        var methodSymbol = isTargetTypeSymbol ? context.SemanticModel.GetDeclaredSymbol(methodDeclarationSyntax, ct) : context.TargetSymbol as IMethodSymbol;
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

        var createSyncVersionAttribute = context.SemanticModel.Compilation.GetTypeByMetadataName(QualifiedCreateSyncVersionAttribute);
        if (createSyncVersionAttribute == null)
        {
            // nothing to do if this type isn't available
            return null;
        }

        var ignoreCreateSyncVersionAttribute = context.SemanticModel.Compilation.GetTypeByMetadataName(QualifiedIgnoreCreateSyncVersionAttribute);

        foreach (var attributeData in methodSymbol.GetAttributes())
        {
            if (ignoreCreateSyncVersionAttribute != null && ignoreCreateSyncVersionAttribute.Equals(attributeData.AttributeClass, SymbolEqualityComparer.Default))
            {
                // Skip processing if the method has the ignored attribute applied
                return null;
            }

            if (isTargetTypeSymbol && createSyncVersionAttribute.Equals(attributeData.AttributeClass, SymbolEqualityComparer.Default))
            {
                // Skip processing if the attribute is defined on the type to prioritize method-level attribute
                return null;
            }
        }

        AttributeData syncMethodGeneratorAttributeData = null!;
        foreach (var attributeData in context.TargetSymbol.GetAttributes())
        {
            if (!createSyncVersionAttribute.Equals(attributeData.AttributeClass, SymbolEqualityComparer.Default))
            {
                continue;
            }

            syncMethodGeneratorAttributeData = attributeData;
            break;
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

        var classes = ImmutableArray.CreateBuilder<MethodParentDeclaration>();
        SyntaxNode? node = methodDeclarationSyntax;
        while (node.Parent is not null)
        {
            node = node.Parent;
            MethodParentDeclaration? mpd = node switch
            {
                ClassDeclarationSyntax o => new(MethodParent.Class, o.Identifier, o.Modifiers, o.TypeParameterList),
                StructDeclarationSyntax o => new(MethodParent.Struct, o.Identifier, o.Modifiers, o.TypeParameterList),
                RecordDeclarationSyntax o => new(MethodParent.Record, o.Identifier, o.Modifiers, o.TypeParameterList, o.ClassOrStructKeyword),
                InterfaceDeclarationSyntax o => new(MethodParent.Interface, o.Identifier, o.Modifiers, o.TypeParameterList),
                _ => null,
            };

            if (mpd is null)
            {
                break;
            }

            classes.Insert(0, mpd);
        }

        if (classes.Count == 0)
        {
            return null;
        }

        var explicitDisableNullable = syncMethodGeneratorAttributeData.NamedArguments.FirstOrDefault(c => c.Key == OmitNullableDirective) is { Value.Value: true };
        disableNullable |= explicitDisableNullable;

        var preserveProgress = syncMethodGeneratorAttributeData.NamedArguments.FirstOrDefault(c => c.Key == PreserveProgress) is { Value.Value: true };

        var rewriter = new AsyncToSyncRewriter(context.SemanticModel, disableNullable, preserveProgress);
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
            while (node is not null and not CompilationUnitSyntax)
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

        var result = new MethodToGenerate(index, namespaces.ToImmutable(), isNamespaceFileScoped, classes.ToImmutable(), methodDeclarationSyntax.Identifier.ValueText, content, disableNullable, rewriter.Diagnostics, hasErrors);

        return result;
    }

    private static (MethodToGenerate MethodToGenerate, string Path, string Content) GenerateSource(MethodToGenerate m)
    {
        static string BuildClassName(MethodParentDeclaration c)
            => c.TypeParameterListSyntax.IsEmpty
                ? c.ParentName
                : c.ParentName + "{" + string.Join(",", c.TypeParameterListSyntax) + "}";

        var sourcePath = $"{string.Join(".", m.Namespaces)}" +
            $".{string.Join(".", m.Parents.Select(BuildClassName))}" +
            $".{m.MethodName + (m.Index == 1 ? string.Empty : "_" + m.Index)}.g.cs";

        var source = SourceGenerationHelper.GenerateExtensionClass(m);

        return (m, sourcePath, source);
    }

    internal record TransformResult(GeneratorAttributeSyntaxContext Context, MethodDeclarationSyntax Syntax);
}
