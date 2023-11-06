//HintName: Test.Class.GetAllTextAsync.g.cs
public static string GetAllText(string fileName)
{
    // Eager argument validation
    if (string.IsNullOrEmpty(fileName)) throw new global::System.ArgumentNullException(nameof(fileName));
    return GetAllText();

    string GetAllText()
    {
        var result = global::System.IO.File.ReadAllText(fileName);
        return result;
    }
}
