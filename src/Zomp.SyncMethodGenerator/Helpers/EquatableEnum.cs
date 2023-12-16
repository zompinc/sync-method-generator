namespace Zomp.SyncMethodGenerator.Helpers;

/// <summary>
/// An equatable enum. This is equivalent to <see cref="Enum"/> but with value equality support.
/// </summary>
/// <remarks>
/// Initializes a new instance of the <see cref="EquatableEnum{T}"/> struct.
/// </remarks>
internal readonly struct EquatableEnum<T>(T value) : IEquatable<EquatableEnum<T>>
    where T : struct, Enum
{
    /// <summary>
    /// Gets the underlying value.
    /// </summary>
    public T Value { get; } = value;

    /// <summary>
    /// Implicitly converts an <see cref="EquatableEnum{T}"/> to <typeparamref name="T"/>.
    /// </summary>
    /// <returns>An <typeparamref name="T"/> instance from a given <see cref="EquatableEnum{T}"/>.</returns>
    public static implicit operator T(EquatableEnum<T> value) => value.Value;

    /// <summary>
    /// Implicitly converts an <typeparamref name="T"/> to <see cref="EquatableEnum{T}"/>.
    /// </summary>
    /// <returns>An <see cref="EquatableEnum{T}"/> instance from a given <typeparamref name="T"/>.</returns>
    public static implicit operator EquatableEnum<T>(T value) => new(value);

    /// <summary>
    /// Checks whether two <see cref="EquatableEnum{T}"/> values are equal.
    /// </summary>
    /// <param name="left">The first <see cref="EquatableEnum{T}"/> value.</param>
    /// <param name="right">The second <see cref="EquatableEnum{T}"/> value.</param>
    /// <returns>Whether <paramref name="left"/> and <paramref name="right"/> are equal.</returns>
    public static bool operator ==(EquatableEnum<T> left, EquatableEnum<T> right) => left.Equals(right);

    /// <summary>
    /// Checks whether two <see cref="EquatableEnum{T}"/> values are not equal.
    /// </summary>
    /// <param name="left">The first <see cref="EquatableEnum{T}"/> value.</param>
    /// <param name="right">The second <see cref="EquatableEnum{T}"/> value.</param>
    /// <returns>Whether <paramref name="left"/> and <paramref name="right"/> are not equal.</returns>
    public static bool operator !=(EquatableEnum<T> left, EquatableEnum<T> right) => !left.Equals(right);

    /// <inheritdoc />
    public bool Equals(EquatableEnum<T> other) => Value.Equals(other.Value);

    /// <inheritdoc />
    public override bool Equals(object? obj) => obj is EquatableEnum<T> other && Equals(other);

    /// <inheritdoc />
    public override int GetHashCode() => Value.GetHashCode();
}
