namespace Zomp.SyncMethodGenerator.Visitors;

internal sealed class BreakVisitor : CSharpSyntaxVisitor<bool>
{
    public static readonly BreakVisitor Instance = new();

    public override bool VisitBreakStatement(BreakStatementSyntax node) => true;

    public override bool VisitGotoStatement(GotoStatementSyntax node) => false;

    public override bool VisitWhileStatement(WhileStatementSyntax node) => false;

    public override bool VisitDoStatement(DoStatementSyntax node) => false;

    public override bool VisitForStatement(ForStatementSyntax node) => false;

    public override bool VisitForEachStatement(ForEachStatementSyntax node) => false;

    public override bool VisitForEachVariableStatement(ForEachVariableStatementSyntax node) => false;

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
