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

    /// <summary>
    /// Longest generated file name before the scope is shortened. Emitted files sit under
    /// {project}\obj\{configuration}\{framework}\generated\{generator assembly}\{generator},
    /// which alone is around a hundred characters before the project path is counted.
    /// </summary>
    internal const string OmitNullableDirective = "OmitNullableDirective";
    internal const string PreserveProgress = "PreserveProgress";
    internal const string PreserveCancellationToken = "PreserveCancellationToken";

    private const int MaxFileNameLength = 100;

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
        MethodDeclarationSyntax { AttributeLists.Count: > 0 } => true,
        ClassDeclarationSyntax { AttributeLists.Count: > 0 } => true,
        StructDeclarationSyntax { AttributeLists.Count: > 0 } => true,
        InterfaceDeclarationSyntax { AttributeLists.Count: > 0 } => true,
        RecordDeclarationSyntax { AttributeLists.Count: > 0 } => true,
        _ => false,
    };

    private static ImmutableArray<TransformResult> TransformForGeneration(GeneratorAttributeSyntaxContext ctx, CancellationToken ct)
    {
        ct.ThrowIfCancellationRequested();

        if (ctx.TargetNode is TypeDeclarationSyntax typeDecl)
        {
#if ROSLYN_4_12_OR_GREATER
            return [.. typeDecl.Members.OfType<MethodDeclarationSyntax>().Select(s => new TransformResult(ctx, s))];
#else
            return ImmutableArray.CreateRange(typeDecl.Members.OfType<MethodDeclarationSyntax>().Select(s => new TransformResult(ctx, s)));
#endif
        }
        else if (ctx.TargetNode is MethodDeclarationSyntax methodDecl)
        {
#if ROSLYN_4_12_OR_GREATER
            return [new TransformResult(ctx, methodDecl)];
#else
            return ImmutableArray.Create(new TransformResult(ctx, methodDecl));
#endif
        }

#if ROSLYN_4_12_OR_GREATER
        return [];
#else
        return ImmutableArray<TransformResult>.Empty;
#endif
    }

    private static (MethodToGenerate MethodToGenerate, string Path, string Content) GenerateSource(MethodToGenerate m)
    {
        static string BuildClassName(MethodParentDeclaration c)
            => c.TypeParameterListSyntax.IsEmpty
                ? c.ParentName
                : c.ParentName + "{" + string.Join(",", c.TypeParameterListSyntax) + "}";

        var scope = $"{string.Join(".", m.Namespaces)}" +
            $".{string.Join(".", m.Parents.Select(BuildClassName))}" +
            (m.IsCSharp14Extension ? ".ext" : string.Empty);

        var method = m.MethodName + (m.Index == 1 ? string.Empty : "_" + m.Index);

        var sourcePath = BuildFileName(scope, method);

        var source = SourceGenerationHelper.GenerateExtensionClass(m);

        return (m, sourcePath, source);
    }

    /// <summary>
    /// Builds the file name a generated method is emitted under, shortening it when the
    /// namespace and containing type chain make it long enough to be a problem. Emitted files
    /// live several directories deep inside the intermediate output path, so a long name here
    /// can carry the full path past the limit the file system accepts.
    /// </summary>
    /// <remarks>
    /// The scope is what gets shortened, since the method name is what someone reads the file
    /// name for. A hash of the untruncated name keeps distinct methods in distinct files.
    /// </remarks>
    private static string BuildFileName(string scope, string method)
    {
        const string extension = ".g.cs";

        var fileName = $"{scope}.{method}{extension}";

        if (fileName.Length <= MaxFileNameLength)
        {
            return fileName;
        }

        var hash = Hash(fileName);

        // separator, hash, dot, extension
        var fixedLength = 1 + hash.Length + 1 + extension.Length;

        var forScope = MaxFileNameLength - fixedLength - method.Length;

        if (forScope > 0)
        {
            return $"{scope[..Math.Min(scope.Length, forScope)]}_{hash}.{method}{extension}";
        }

        // The method name alone fills the budget, so it has to be shortened as well.
        return $"_{hash}.{method[..(MaxFileNameLength - fixedLength)]}{extension}";
    }

    /// <summary>
    /// FNV-1a. Short, dependency free, and stable across processes and runtimes, which
    /// <see cref="string.GetHashCode()"/> is not.
    /// </summary>
    private static string Hash(string value)
    {
        const uint offsetBasis = 2166136261;
        const uint prime = 16777619;

        var hash = offsetBasis;

        unchecked
        {
            foreach (var c in value)
            {
                hash ^= c;
                hash *= prime;
            }
        }

        return hash.ToString("x8", System.Globalization.CultureInfo.InvariantCulture);
    }

    private static MethodToGenerate? GetMethodToGenerate(GeneratorAttributeSyntaxContext context, MethodDeclarationSyntax methodDeclarationSyntax, bool disableNullable, CancellationToken ct)
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

        var syncMethodGeneratorAttributeData = context.Attributes[0];

        var classes = ImmutableArray.CreateBuilder<MethodParentDeclaration>();
        SyntaxNode? node = methodDeclarationSyntax;
#if ROSLYN_5_0_OR_GREATER
        ExtensionBlockDeclarationSyntax? extensionParent = null;
#endif
        while (node.Parent is not null)
        {
            node = node.Parent;
#if ROSLYN_5_0_OR_GREATER
            if (node is ExtensionBlockDeclarationSyntax eds)
            {
                extensionParent = eds;
                continue;
            }
#endif

            MethodParentDeclaration? mpd = node switch
            {
                ClassDeclarationSyntax o => new(MethodParent.Class, o.Identifier, o.Modifiers, o.TypeParameterList),
                StructDeclarationSyntax o => new(MethodParent.Struct, o.Identifier, o.Modifiers, o.TypeParameterList),
                RecordDeclarationSyntax o => new(MethodParent.Record, o.Identifier, o.Modifiers, o.TypeParameterList, o.ClassOrStructKeyword),
                InterfaceDeclarationSyntax o => new(MethodParent.Interface, o.Identifier, o.Modifiers, o.TypeParameterList),
                ////ExtensionBlockDeclarationSyntax o => new(MethodParent.ExtensionDeclaration, o.Identifier, o.Modifiers, o.TypeParameterList),
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
        var preserveCancellationToken = syncMethodGeneratorAttributeData.NamedArguments.FirstOrDefault(c => c.Key == PreserveCancellationToken) is { Value.Value: true };

#if ROSLYN_5_0_OR_GREATER
        var toVisit = extensionParent ?? (SyntaxNode)methodDeclarationSyntax;
#else
        var toVisit = (SyntaxNode)methodDeclarationSyntax;
#endif
        var rewriter = new AsyncToSyncRewriter(context.SemanticModel, disableNullable, preserveProgress, preserveCancellationToken, methodDeclarationSyntax);
        var sn = rewriter.Visit(toVisit);
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

#if ROSLYN_5_0_OR_GREATER
        var isCSharp14Extension = extensionParent is not null;
#else
        var isCSharp14Extension = false;
#endif
        var result = new MethodToGenerate(index, namespaces.ToImmutable(), isNamespaceFileScoped, isCSharp14Extension, classes.ToImmutable(), methodDeclarationSyntax.Identifier.ValueText, content, disableNullable, rewriter.Diagnostics, hasErrors);

        return result;
    }

    internal sealed record TransformResult(GeneratorAttributeSyntaxContext Context, MethodDeclarationSyntax Syntax);
}
