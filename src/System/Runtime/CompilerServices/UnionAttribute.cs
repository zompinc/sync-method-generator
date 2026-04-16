namespace System.Runtime.CompilerServices;

/// <summary>
/// Marks a type as a C# union.
/// </summary>
[AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, AllowMultiple = false)]
public sealed class UnionAttribute : Attribute;
