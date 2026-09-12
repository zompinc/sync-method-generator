namespace System.Runtime.CompilerServices;

/// <summary>
/// Marks a type as a C# union.
/// </summary>
[AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, AllowMultiple = false, Inherited = false)]
internal sealed class UnionAttribute : Attribute;
