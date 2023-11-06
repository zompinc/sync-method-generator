//HintName: Test.Class.ExecAsync.g.cs
public void Exec()
{
#if SYMBOL1
    ArgumentNullException.ThrowIfNull(source);
#elif SYMBOL2
    ArgumentNullException.ThrowIfNull(source);
#else
    global::System.ArgumentNullException.ThrowIfNull(source);
#endif
}
