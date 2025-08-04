using Microsoft.CodeAnalysis.Text;

namespace Generator.Tests;

public class MemoryAdditionalText(string path, string text) : AdditionalText
{
    public override string Path { get; } = path;

    public override SourceText GetText(CancellationToken cancellationToken = default)
    {
        return SourceText.From(text);
    }
}
