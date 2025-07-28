//HintName: Test.Class.MethodAsync.g.cs
public void Method(global::System.IO.Stream stream)
{
    var buffer = global::Test.Class.GiveStaticMem().Span;
    stream.Write(buffer);
}
