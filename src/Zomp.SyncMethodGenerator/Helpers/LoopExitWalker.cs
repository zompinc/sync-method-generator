namespace Zomp.SyncMethodGenerator.Helpers;

/// <summary>
/// Finds the ways out of a loop body. Code which runs somewhere else, in a lambda or a local
/// function, is skipped, and a <c>break</c> only counts when it belongs to the loop itself.
/// </summary>
/// <param name="methodsWhichNeverReturn">Names of the methods whose calls never return.</param>
internal sealed class LoopExitWalker(ISet<string> methodsWhichNeverReturn) : CSharpSyntaxWalker
{
    private int nestedBreakTargets;

    /// <summary>
    /// Gets a value indicating whether a <c>break</c> leaves the loop.
    /// </summary>
    public bool Breaks { get; private set; }

    /// <summary>
    /// Gets a value indicating whether the loop contains a <c>goto</c>, which may leave it.
    /// </summary>
    public bool Jumps { get; private set; }

    /// <summary>
    /// Gets a value indicating whether the loop can leave the method: <c>return</c>,
    /// <c>throw</c>, <c>yield break</c>, or a call which never returns.
    /// </summary>
    public bool LeavesMethod { get; private set; }

    /// <inheritdoc/>
    public override void VisitBreakStatement(BreakStatementSyntax node) => Breaks |= nestedBreakTargets == 0;

    /// <inheritdoc/>
    public override void VisitGotoStatement(GotoStatementSyntax node) => Jumps = true;

    /// <inheritdoc/>
    public override void VisitReturnStatement(ReturnStatementSyntax node) => LeavesMethod = true;

    /// <inheritdoc/>
    public override void VisitThrowStatement(ThrowStatementSyntax node) => LeavesMethod = true;

    /// <inheritdoc/>
    public override void VisitThrowExpression(ThrowExpressionSyntax node) => LeavesMethod = true;

    /// <inheritdoc/>
    public override void VisitYieldStatement(YieldStatementSyntax node) => LeavesMethod |= node.IsKind(SyntaxKind.YieldBreakStatement);

    /// <inheritdoc/>
    public override void VisitInvocationExpression(InvocationExpressionSyntax node)
    {
        var name = node.Expression switch
        {
            MemberAccessExpressionSyntax member => member.Name.Identifier.ValueText,
            SimpleNameSyntax simple => simple.Identifier.ValueText,
            _ => null,
        };

        // A static call is rewritten into one qualified identifier, so only its last part names the method
        if (name is not null && methodsWhichNeverReturn.Contains(name[(name.LastIndexOf('.') + 1)..]))
        {
            LeavesMethod = true;
        }

        base.VisitInvocationExpression(node);
    }

    /// <inheritdoc/>
    public override void VisitWhileStatement(WhileStatementSyntax node) => VisitBreakTarget(node, base.VisitWhileStatement);

    /// <inheritdoc/>
    public override void VisitDoStatement(DoStatementSyntax node) => VisitBreakTarget(node, base.VisitDoStatement);

    /// <inheritdoc/>
    public override void VisitForStatement(ForStatementSyntax node) => VisitBreakTarget(node, base.VisitForStatement);

    /// <inheritdoc/>
    public override void VisitForEachStatement(ForEachStatementSyntax node) => VisitBreakTarget(node, base.VisitForEachStatement);

    /// <inheritdoc/>
    public override void VisitForEachVariableStatement(ForEachVariableStatementSyntax node) => VisitBreakTarget(node, base.VisitForEachVariableStatement);

    /// <inheritdoc/>
    public override void VisitSwitchStatement(SwitchStatementSyntax node) => VisitBreakTarget(node, base.VisitSwitchStatement);

    /// <inheritdoc/>
    public override void VisitSimpleLambdaExpression(SimpleLambdaExpressionSyntax node)
    {
    }

    /// <inheritdoc/>
    public override void VisitParenthesizedLambdaExpression(ParenthesizedLambdaExpressionSyntax node)
    {
    }

    /// <inheritdoc/>
    public override void VisitAnonymousMethodExpression(AnonymousMethodExpressionSyntax node)
    {
    }

    /// <inheritdoc/>
    public override void VisitLocalFunctionStatement(LocalFunctionStatementSyntax node)
    {
    }

    private void VisitBreakTarget<TNode>(TNode node, Action<TNode> visit)
    {
        ++nestedBreakTargets;
        visit(node);
        --nestedBreakTargets;
    }
}
