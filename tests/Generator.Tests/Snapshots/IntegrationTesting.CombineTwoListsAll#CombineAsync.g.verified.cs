//HintName: Test.Class.CombineAsync.g.cs
public static global::System.Collections.Generic.IList<(TLeft Left, TRight Right)> Combine<TLeft, TRight>(this global::System.Collections.Generic.IList<TLeft> list1, global::System.Collections.Generic.IList<TRight> list2)
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
