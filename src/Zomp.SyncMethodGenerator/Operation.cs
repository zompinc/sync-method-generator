namespace Zomp.SyncMethodGenerator;

/// <summary>
/// A change to a list rewritten around SYNC_ONLY directives: either the statements to insert at a
/// position, or <see langword="true"/> to remove the <c>#endif</c> which leads the item there.
/// </summary>
internal readonly union Operation(List<StatementSyntax>, bool);
