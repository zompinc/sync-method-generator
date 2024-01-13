//HintName: Test.Class.SwitchAsync.g.cs
public void Switch(global::System.IO.Stream stream)
{
    var s = stream switch
    {
        global::System.IO.FileStream fs => fs,
        _ => throw new global::System.InvalidOperationException("No"),
    };
}
