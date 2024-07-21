namespace Generator.Tests;

public class PreprocessorTests
{
    [Fact]
    public Task MacrosAroundBraces() => $$"""
#if !BLA
{
#endif

#if !BLA
}
#endif
""".Verify(sourceType: SourceType.MethodBody);
}
