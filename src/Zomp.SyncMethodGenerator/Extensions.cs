using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Zomp.SyncMethodGenerator;

internal static class Extensions
{
    extension(INamedTypeSymbol symbol)
    {
        public bool IsMemory => symbol is
        {
            Name: nameof(Memory<>) or nameof(ReadOnlyMemory<>), IsGenericType: true, TypeArguments: [{ }],
            ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true }
        };

        public bool IsMemoryOrNullableMemory => symbol.IsMemory || symbol is
        {
            Name: nameof(Nullable<>), IsGenericType: true, TypeArguments: [INamedTypeSymbol { IsMemory: true }],
            ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true }
        };

        public bool IsTaskOrValueTask => symbol is
        {
            Name: Task or ValueTask,
            ContainingNamespace: { Name: Tasks, ContainingNamespace: { Name: Threading, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
        };

        public bool IsNonGenericTaskOrValueTask => symbol is
        {
            IsTaskOrValueTask: true, IsGenericType: false,
        };

        public bool IsValueTask => symbol is
        {
            Name: ValueTask,
            ContainingNamespace: { Name: Tasks, ContainingNamespace: { Name: Threading, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
        };

        public bool IsNonGenericValueTask => symbol is
        {
            IsValueTask: true, IsGenericType: false,
        };

        public bool IsSystemFunc => symbol is
        {
            Name: Func, IsGenericType: true, TypeArguments: [{ }, ..],
            ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true }
        };

        public bool IsCancellationToken => symbol is
        {
            Name: CancellationToken, IsGenericType: false,
            ContainingNamespace: { Name: Threading, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } }
        };

        public bool IsIProgress => symbol is
        {
            Name: IProgress, IsGenericType: true,
            ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true }
        };

        public bool IsIAsyncEnumerableOrIAsyncEnumerator => symbol is
        {
            Name: IAsyncEnumerable or IAsyncEnumerator, IsGenericType: true,
            ContainingNamespace: { Name: Generic, ContainingNamespace: { Name: Collections, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
        };

        public bool IsEnumerator => symbol is
        {
            Name: Enumerator, ContainingType:
            {
                IsGenericType: true, TypeArguments: [{ }],
                ContainingNamespace: { Name: CompilerServices, ContainingNamespace: { Name: Runtime, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
            }
        };
    }

    extension(IMethodSymbol symbol)
    {
        public bool IsAsTask => symbol is
        {
            Name: AsTask, ContainingType:
            {
                Name: ValueTask,
                ContainingNamespace: { Name: Tasks, ContainingNamespace: { Name: Threading, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
            }
        };

        public bool IsTaskExtension => symbol.ReturnType is
        {
            Name: nameof(ConfiguredTaskAwaitable) or nameof(ConfiguredValueTaskAwaitable) or nameof(ConfiguredCancelableAsyncEnumerable<>) or nameof(ConfiguredAsyncDisposable),
            ContainingNamespace: { Name: CompilerServices, ContainingNamespace: { Name: Runtime, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } } }
        };
    }

    extension(IPropertySymbol symbol)
    {
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Performance", "CA1822:Mark members as static", Justification = "False positive")]
        public bool IsCancellationRequested => symbol is
        {
            Name: IsCancellationRequested, ContainingType:
            {
                Name: CancellationToken, IsGenericType: false,
                ContainingNamespace: { Name: Threading, ContainingNamespace: { Name: System, ContainingNamespace.IsGlobalNamespace: true } }
            }
        };
    }

    // Namespace parts
    private const string Func = "Func";
    private const string System = "System";
    private const string Threading = "Threading";
    private const string Tasks = "Tasks";
    private const string Generic = "Generic";
    private const string Collections = "Collections";
    private const string CompilerServices = "CompilerServices";
    private const string Runtime = "Runtime";

    // Type names
    private const string Enumerator = nameof(Span<>.Enumerator);
    private const string CancellationToken = nameof(global::System.Threading.CancellationToken);
    private const string IAsyncEnumerable = nameof(IAsyncEnumerable<>);
    private const string IAsyncEnumerator = nameof(IAsyncEnumerator<>);
    private const string Task = nameof(Task<>);
    private const string ValueTask = nameof(ValueTask<>);
    private const string IProgress = nameof(IProgress<>);

    // Members
    private const string IsCancellationRequested = nameof(global::System.Threading.CancellationToken.IsCancellationRequested);
    private const string AsTask = nameof(global::System.Threading.Tasks.ValueTask.AsTask);

    /// <summary>
    /// Gets array of indices that match a predicate.
    /// </summary>
    /// <typeparam name="T">The type.</typeparam>
    /// <param name="elements">Original elements.</param>
    /// <param name="predicate">The predicate.</param>
    /// <returns>Array of indices that match a predicate.</returns>
    internal static int[] GetIndices<T>(this IEnumerable<T> elements, Func<T, int, bool> predicate)
    => [.. elements.Select((ps, i) => (ps, i))
        .Where((elem, i) => predicate(elem.ps, i))
        .Select(t => t.i)];

    internal static int[] GetIndices<T>(this IEnumerable<T> elements, Func<T, bool> predicate)
        => elements.GetIndices((e, _) => predicate(e));

    internal static bool EndsWithAsync(this string str)
        => str.EndsWith("Async", StringComparison.Ordinal);

    internal static SyntaxToken PrependSpace(this SyntaxToken token)
        => token.WithLeadingTrivia(Space);

    internal static SyntaxToken AppendSpace(this SyntaxToken token)
        => token.WithTrailingTrivia(Space);

    internal static T PrependSpace<T>(this T syntax)
        where T : ExpressionSyntax
        => syntax.WithLeadingTrivia(Space);

    internal static T AppendSpace<T>(this T syntax)
        where T : ExpressionSyntax
        => syntax.WithTrailingTrivia(Space);

    internal static SimpleNameSyntax ChangeIdentifier(this MemberAccessExpressionSyntax mae, string id)
    {
        return mae is { Name: GenericNameSyntax gns }
            ? GenericName(Identifier(id), gns.TypeArgumentList)
            : IdentifierName(Identifier(id));
    }

    internal static SimpleNameSyntax ChangeIdentifier(this MemberBindingExpressionSyntax mae, string id)
    {
        return mae is { Name: GenericNameSyntax gns }
            ? GenericName(Identifier(id), gns.TypeArgumentList)
            : IdentifierName(Identifier(id));
    }

    internal static BlockSyntax CreateBlock(this ICollection<StatementSyntax> statements, int indentationLevel = 0)
    {
        var list = new List<StatementSyntax>();

        var i = 0;
        foreach (var statement in statements)
        {
            var leading = new List<SyntaxTrivia>();
            var trailing = new List<SyntaxTrivia>() { CarriageReturnLineFeed };

            if (i == 0)
            {
                leading.Add(CarriageReturnLineFeed);
            }

            if (statement is IfStatementSyntax && i < statements.Count - 1)
            {
                trailing.Add(CarriageReturnLineFeed);
            }

            if (i == statements.Count - 1)
            {
                trailing.Add(Whitespace(new string(' ', 4 * indentationLevel)));
            }

            if (indentationLevel > 0)
            {
                leading.Add(Whitespace(new string(' ', 4 * (indentationLevel + 1))));
            }

            list.Add(statement.WithTrailingTrivia(trailing)
                .WithLeadingTrivia(leading));
            ++i;
        }

        return Block(List(list))
            .WithLeadingTrivia(CarriageReturnLineFeed, Whitespace(new string(' ', 4 * indentationLevel)));
    }

    internal static bool InitializedToNull(this VariableDeclaratorSyntax node)
        => node.Initializer?.Value is LiteralExpressionSyntax { RawKind: (int)SyntaxKind.NullLiteralExpression }
            or PostfixUnaryExpressionSyntax { RawKind: (int)SyntaxKind.SuppressNullableWarningExpression };
}
