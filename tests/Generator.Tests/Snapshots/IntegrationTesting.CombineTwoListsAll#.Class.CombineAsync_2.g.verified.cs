﻿//HintName: .Class.CombineAsync_2.g.cs
// <auto-generated/>
#nullable enable
partial class Class
{
    public static global::System.ReadOnlySpan<(TLeft Left, TRight Right)> Combine<TLeft, TRight>(this global::System.ReadOnlySpan<TLeft> list1, global::System.ReadOnlySpan<TRight> list2)
    {
        using var enumerator2 = list2.GetEnumerator();
        foreach (var item in list1)
        {
            if (!enumerator2.MoveNext())
            {
                throw new global::System.InvalidOperationException("Must have the same size");
            }
            yield return (item, enumerator2.Current);
        }
    }
}
