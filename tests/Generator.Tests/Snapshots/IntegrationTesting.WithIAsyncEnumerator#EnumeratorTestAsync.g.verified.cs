//HintName: Test.Class.EnumeratorTestAsync.g.cs
void EnumeratorTest(global::System.Collections.Generic.IEnumerable<int> range)
{
    global::System.Collections.Generic.IEnumerator<int> e = range.GetEnumerator();
    try
    {
        while (e.MoveNext()) global::System.Console.Write(e.Current + " ");
    }
    finally { if (e != null) e.Dispose(); }
}
