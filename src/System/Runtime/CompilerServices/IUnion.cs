namespace System.Runtime.CompilerServices;

/// <summary>
/// Exposes the value held by a C# union.
/// </summary>
internal interface IUnion
{
    /// <summary>
    /// Gets the value held by the union, or <see langword="null"/> when it holds none.
    /// </summary>
    object? Value { get; }
}
