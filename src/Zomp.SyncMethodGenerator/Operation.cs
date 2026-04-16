namespace Zomp.SyncMethodGenerator;

/// <summary>
/// Represents a sync-only operation.
/// </summary>
[Union]
internal readonly struct Operation
{
    /// <summary>
    /// Initializes a new instance of the <see cref="Operation"/> struct.
    /// </summary>
    /// <param name="value">The new statements to insert.</param>
    public Operation(List<StatementSyntax> value)
    {
        Value = value;
    }

    /// <summary>
    /// Initializes a new instance of the <see cref="Operation"/> struct.
    /// </summary>
    /// <param name="value">A value indicating whether to remove the leading end-if directive.</param>
    public Operation(bool value)
    {
        Value = value;
    }

    /// <summary>
    /// Gets the active operation value.
    /// </summary>
    public object? Value { get; }

    /// <summary>
    /// Gets a value indicating whether the operation inserts new statements.
    /// </summary>
    public bool IsNewStatements => Value is List<StatementSyntax>;

    /// <summary>
    /// Gets the operation value as new statements.
    /// </summary>
    public List<StatementSyntax> AsNewStatements => (List<StatementSyntax>)Value!;
}
