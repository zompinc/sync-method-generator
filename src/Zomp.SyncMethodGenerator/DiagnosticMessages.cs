namespace Zomp.SyncMethodGenerator;

internal static class DiagnosticMessages
{
    internal static readonly DiagnosticDescriptor InvalidNesting = new(
        id: "ZSMGEN001",
        title: $"{AsyncToSyncRewriter.SyncOnly} and !{AsyncToSyncRewriter.SyncOnly} cannot be nested inside each other",
        messageFormat: $"Couldn't process directive '{{0}}'. {AsyncToSyncRewriter.SyncOnly} and !{AsyncToSyncRewriter.SyncOnly} cannot be nested inside each other.",
        category: Preprocessor,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    internal static readonly DiagnosticDescriptor InvalidCondition = new(
        id: "ZSMGEN002",
        title: $"{AsyncToSyncRewriter.SyncOnly} and other symbols must not be in the same condition",
        messageFormat: $"Couldn't process condition '{{0}}'. {AsyncToSyncRewriter.SyncOnly} and other symbols must not be in the same condition.",
        category: Preprocessor,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    internal static readonly DiagnosticDescriptor InvalidElif = new(
        id: "ZSMGEN003",
        title: $"{AsyncToSyncRewriter.SyncOnly} cannot be used with #elif",
        messageFormat: $"Couldn't process directive '{{0}}'. {AsyncToSyncRewriter.SyncOnly} cannot be used with #elif.",
        category: Preprocessor,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    private const string Preprocessor = "Preprocessor";
}
