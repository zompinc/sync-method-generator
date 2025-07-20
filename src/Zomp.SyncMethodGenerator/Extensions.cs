using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Zomp.SyncMethodGenerator;

internal static class Extensions
{
    extension(INamedTypeSymbol symbol)
    {
        public bool IsMemory => symbol is
        {
            Name: nameof(Memory<>) or nameof(ReadOnlyMemory<>), IsGenericType: true, TypeArguments: [{ }],
            ContainingNamespace: { Name: "System", ContainingNamespace.IsGlobalNamespace: true }
        };

        public bool IsMemoryOrNullableMemory => symbol.IsMemory || symbol is
        {
            Name: nameof(Nullable<>), IsGenericType: true, TypeArguments: [INamedTypeSymbol { IsMemory: true }],
            ContainingNamespace: { Name: "System", ContainingNamespace.IsGlobalNamespace: true }
        };
    }

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
}
