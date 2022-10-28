namespace Zomp.SyncMethodGenerator;
internal class DiagnosticMessages
{
    internal static readonly DiagnosticDescriptor ExceptionOccured = new(
        id: "ZSMGEN001",
        title: "Exception occured",
        messageFormat: "Couldn't process method file '{0}'.",
        category: "InternalError",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    internal static readonly DiagnosticDescriptor ClassMustBePartial = new(
        id: "ZSMGEN002",
        title: "Mark partial",
        messageFormat: "Class {0} must be partial to generate code.",
        category: "InternalError",
        DiagnosticSeverity.Warning,
        isEnabledByDefault: true);


    internal static readonly DiagnosticDescriptor CannotRetrieveSymbol = new(
        id: "ZSMGEN003",
        title: "Cannot retrieve symbol",
        messageFormat: "Cannot retrieve symbol from type '{0}'. Exception: {1}",
        category: "InternalError",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);
}
