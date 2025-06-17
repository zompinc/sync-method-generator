//HintName: Test.Class.CallWithIProgressAsync.g.cs
public static void CallWithIProgress()
{
    global::Test.Class.CustomProgress progress = new();

    switch (global::Test.Class.k)
{
    case 1:
        progress++;
        break;
    default:
        progress++;
        progress++;
        break;
}

    WithIProgress(progress);
}
