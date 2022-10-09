public static class ModuleInitializer
{
#if NET5_0_OR_GREATER
    [System.Runtime.CompilerServices.ModuleInitializer]
#endif
    public static void Initialize()
    {
        VerifySourceGenerators.Enable();
    }
}
