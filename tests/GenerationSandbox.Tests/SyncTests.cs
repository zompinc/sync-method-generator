#if NET6_0_OR_GREATER
using System.Linq;
#endif
using Xunit;

namespace GenerationSandbox.Tests;

public class SyncTests
{
    [Fact]
    public void TestGeneratedAverageRoutine()
    {
        var expected = new double[] { 9, 9.5, 8, 7.333333333333333, 5, 4, 2 };
        var myNumbers = new double[] { 9, 10, 5, 7, 3, 2, 1 };
        var result = myNumbers.MovingAverage(3);
        Assert.Equal(expected, result);
    }

    [Fact]
    public void TestStaticAsyncWithIProgress()
    {
        AsyncWithIProgress.CallWithIProgress();
    }

#if NET6_0_OR_GREATER
    [Fact]
    public void TestIndexOfMaxSoFar()
    {
        var expected = new int[] { 0, 1, 4 };
        var myNumbers = new int[] { 9, 10, 5, 7, 11, 2, 1 };
        var result = myNumbers.IndexOfMaxSoFar().ToArray();
        Assert.Equal(expected, result);
    }
#endif
}
