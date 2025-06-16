//HintName: Test.Class.CallWithIProgressAsync.g.cs
public static void CallWithIProgress()
{
    var progress = new global::System.Progress<float>();
    WithIProgress(someBool ? progress : null);
}
