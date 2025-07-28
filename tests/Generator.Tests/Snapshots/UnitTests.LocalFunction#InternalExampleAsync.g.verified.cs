//HintName: Test.Class.InternalExampleAsync.g.cs
public static int InternalExample(global::System.IO.Stream stream)
{
    static int Internal(global::System.IO.Stream stream)
    {
        var buf = new byte[1];
        return stream.Read(buf, 0, 1);
    }
    return Internal(stream);
}
