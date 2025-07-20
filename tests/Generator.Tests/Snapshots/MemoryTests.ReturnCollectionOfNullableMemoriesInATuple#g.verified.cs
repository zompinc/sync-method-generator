//HintName: Test.Class.MethodAsync.g.cs
public static global::System.Collections.Generic.IEnumerable<(global::System.Memory<bool> Mem, int Num)> Method<T>(global::System.Collections.Generic.IEnumerable<global::System.Memory<bool>> input)
{
    foreach (global::System.Memory<bool>? elem in input)
    {
        yield return (elem.Value, 1);
    }
}
