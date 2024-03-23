namespace Zomp.SyncMethodGenerator.Visitors;

internal sealed class StopIterationVisitor(SemanticModel semanticModel) : CSharpSyntaxVisitor<bool>
{
    public override bool VisitReturnStatement(ReturnStatementSyntax node) => true;

    public override bool VisitThrowExpression(ThrowExpressionSyntax node) => true;

    public override bool VisitInvocationExpression(InvocationExpressionSyntax node)
    {
        var methodSymbol = semanticModel.GetSymbolInfo(node).Symbol as IMethodSymbol;

        if (methodSymbol?.GetAttributes().Any(a => a.AttributeClass?.Name == "DoesNotReturnAttribute") ?? false)
        {
            return true;
        }

        return base.VisitInvocationExpression(node);
    }

    public override bool DefaultVisit(SyntaxNode node)
    {
        foreach (var child in node.ChildNodes())
        {
            if (Visit(child))
            {
                return true;
            }
        }

        return false;
    }
}
