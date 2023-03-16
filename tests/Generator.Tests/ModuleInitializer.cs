namespace Generator.Tests;

public static class ModuleInitializer
{
    [System.Runtime.CompilerServices.ModuleInitializer]
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Usage", "CA2255:The 'ModuleInitializer' attribute should not be used in libraries", Justification = "This is a testing project")]
    public static void Initialize()
    {
        VerifySourceGenerators.Enable();
    }
}
