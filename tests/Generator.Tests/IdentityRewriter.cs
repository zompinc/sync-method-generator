using Microsoft.CodeAnalysis.CSharp.Syntax;
using Zomp.MethodCloning;

namespace Generator.Tests;

/// <summary>
/// A transformation which changes nothing but the name of the method, leaving everything else
/// to <see cref="CloningRewriter"/>.
/// </summary>
/// <param name="semanticModel">The semantic model.</param>
/// <param name="targetMethod">The method declaration to clone.</param>
internal sealed class IdentityRewriter(SemanticModel semanticModel, MethodDeclarationSyntax targetMethod)
    : CloningRewriter(semanticModel, targetMethod)
{
    /// <summary>
    /// Appended to the name of each method, so that the clone does not collide with the original.
    /// </summary>
    internal const string Suffix = "Clone";

    /// <inheritdoc/>
    public override SyntaxNode? VisitMethodDeclaration(MethodDeclarationSyntax node)
    {
        if (base.VisitMethodDeclaration(node) is not MethodDeclarationSyntax clone)
        {
            return null;
        }

        var name = SyntaxFactory.Identifier(node.Identifier.ValueText + Suffix).WithTriviaFrom(clone.Identifier);

        // Directives leading the method belong to the file it was written in.
        return clone
            .WithIdentifier(name)
            .WithLeadingTrivia(RemovePreprocessorDirectives(clone.GetLeadingTrivia()));
    }
}
