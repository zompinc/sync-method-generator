//HintName: Test.Class.SumAsync.g.cs
int Sum(global::System.Collections.Generic.IEnumerable<int?> enumerable)
{
    int sum = 0;

    foreach (int? i in enumerable)
    {
        if (i.HasValue)
        {
            sum += i.Value;
        }
    }
    
    return sum;
}
