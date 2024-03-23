//HintName: Test.Class.SumAsync.g.cs
int Sum(global::System.Collections.Generic.IEnumerable<(int a, int b)> enumerable)
{
    int sum = 0;

    foreach (var (a, b) in enumerable)
    {
        sum += a + b;
    }
    
    return sum;
}
