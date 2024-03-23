namespace Zomp.SyncMethodGenerator.Visitors;

internal sealed class StopIterationVisitor : CSharpSyntaxVisitor<bool>
{
    public static readonly StopIterationVisitor Instance = new();

    public override bool VisitReturnStatement(ReturnStatementSyntax node) => true;

    public override bool VisitThrowExpression(ThrowExpressionSyntax node) => true;

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
