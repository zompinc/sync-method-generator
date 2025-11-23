//HintName: Test.Class.MethodAsync.g.cs
public void Method(string source)
{
#if SYMBOL1
    ArgumentNullException.ThrowIfNull(source);
#elif SYMBOL2
    ArgumentNullException.ThrowIfNull(source);
#else
    global::System.ArgumentNullException.ThrowIfNull(source);
#endif
}
