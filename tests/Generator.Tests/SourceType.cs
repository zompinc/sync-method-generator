namespace Generator.Tests;

public enum SourceType
{
    /// <summary>
    /// Listing of a body of a block method. A single parameter is passed: CancellationToken ct.
    /// </summary>
    MethodBody,

    /// <summary>
    /// Listing of a body of a class.
    /// </summary>
    ClassBody,

    /// <summary>
    /// Listing for the file in full.
    /// </summary>
    Full,
}
