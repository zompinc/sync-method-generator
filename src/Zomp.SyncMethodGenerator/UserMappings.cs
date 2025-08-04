namespace Zomp.SyncMethodGenerator;

internal sealed class UserMappings(
    EquatableArray<(string Key, string Namespace, string Method)> mappings,
    EquatableArray<ReportedDiagnostic> diagnostics) : IEquatable<UserMappings>
{
    [field: MaybeNull]
    public IReadOnlyDictionary<string, (string Namespace, string Method)> Mappings
        => field ??= mappings.ToDictionary(kv => kv.Key, kv => (kv.Namespace, kv.Method));

    public EquatableArray<ReportedDiagnostic> Diagnostics { get; } = diagnostics;

    public bool Equals(UserMappings? other)
    {
        return other is not null &&
               Mappings.Equals(other.Mappings) &&
               Diagnostics.Equals(other.Diagnostics);
    }

    public override bool Equals(object? obj)
    {
        return obj is not null && (ReferenceEquals(this, obj) || (obj is UserMappings other && Equals(other)));
    }

    public override int GetHashCode() => HashCode.Combine(Mappings);

    public bool TryGetValue(IMethodSymbol ms, out (string Namespace, string Method) value)
    {
        if (ms.ContainingType is { } containingType)
        {
            return Mappings.TryGetValue(
                containingType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat) + "." + ms.Name,
                out value);
        }

        return Mappings.TryGetValue(ms.Name, out value);
    }
}
