//HintName: Test.Class.MethodAsync.g.cs
internal int Method(global::System.Collections.Generic.IEnumerable<(global::System.IO.Stream A, int B)> enumerable)
{
    var enumerator = enumerable.GetEnumerator();

    int sum = 0;
    while (enumerator.MoveNext())
    {
        sum += (int)enumerator.Current.A.Length + enumerator.Current.B;
    }

    return sum;
}
