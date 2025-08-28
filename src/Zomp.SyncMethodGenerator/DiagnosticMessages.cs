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

    internal static readonly DiagnosticDescriptor DuplicateUserMapping = new(
        id: "ZSMGEN004",
        title: "Duplicate user mapping",
        messageFormat: "User mapping '{0}' is already defined",
        category: Preprocessor,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    internal static readonly DiagnosticDescriptor AttributeAndUserMappingConflict = new(
        id: "ZSMGEN005",
        title: "Attribute and user mapping conflict",
        messageFormat: "Method '{0}' has both an attribute and a user mapping defined. The user mapping will be used.",
        category: Preprocessor,
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    private const string Preprocessor = "Preprocessor";
}
