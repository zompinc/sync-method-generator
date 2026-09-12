using Zomp.MethodCloning;
using Zomp.SyncMethodGenerator;

namespace Generator.Tests;

/// <summary>
/// Clones every method the sync generator would synchronize, through <see cref="IdentityRewriter"/>,
/// which changes nothing but its name. A clone which does not compile points at the cloning layer
/// rather than at the async to sync rules.
/// </summary>
internal sealed class IdentityCloneGenerator : IIncrementalGenerator
{
    /// <inheritdoc/>
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        var disableNullable = context.CompilationProvider.Select(static (c, _) =>
            c.Options.NullableContextOptions == NullableContextOptions.Disable
            || c is CSharpCompilation { LanguageVersion: < LanguageVersion.CSharp8 });

        var methods = CloneTarget.ForAttribute(context.SyntaxProvider, SyncMethodSourceGenerator.QualifiedCreateSyncVersionAttribute)
            .Combine(disableNullable)
            .Select(static (data, ct) => Clone(data.Left, data.Right, ct)!)
            .Where(static m => m is not null);

        ClonedMethodOutput.Register(context, methods, Zomp.SyncMethodGenerator.DiagnosticMessages.CollidingOverloads);
    }

    private static ClonedMethod? Clone(CloneTarget target, bool disableNullable, CancellationToken ct)
    {
        var context = target.Context;
        var isTargetTypeSymbol = context.TargetSymbol is ITypeSymbol;

        var symbol = isTargetTypeSymbol
            ? context.SemanticModel.GetDeclaredSymbol(target.Syntax, ct)
            : context.TargetSymbol as IMethodSymbol;

        // The same selection the sync generator makes, so that only the methods it copies are cloned.
        if (symbol is null
            || (!symbol.IsAsync
                && (symbol.ReturnType is not INamedTypeSymbol named || !AsyncToSyncRewriter.IsTypeOfInterest(named))))
        {
            return null;
        }

        foreach (var attribute in symbol.GetAttributes())
        {
            var name = attribute.AttributeClass?.ToDisplayString();

            if (name == SyncMethodSourceGenerator.QualifiedSkipSyncVersionAttribute
                || (isTargetTypeSymbol && name == SyncMethodSourceGenerator.QualifiedCreateSyncVersionAttribute))
            {
                return null;
            }
        }

        if (!MethodLocation.TryCreate(target.Syntax, symbol, out var location, out var root))
        {
            return null;
        }

        disableNullable |= context.Attributes[0].NamedArguments
            .Any(static a => a is { Key: SyncMethodSourceGenerator.OmitNullableDirective, Value.Value: true });

        var rewritten = new IdentityRewriter(context.SemanticModel, target.Syntax).Visit(root);

        return ClonedMethod.Create(location, target.Syntax, rewritten, disableNullable, []);
    }
}
