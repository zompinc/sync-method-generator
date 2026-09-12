namespace Zomp.SyncMethodGenerator.Cloning;

/// <summary>
/// Everything around a method which a copy of it needs in order to compile in a file of its own:
/// the namespaces and types it is nested in, and the using directives in scope where it was written.
/// </summary>
/// <param name="Index">Position of the method among the members of its type which share its name, starting at 1.</param>
/// <param name="Namespaces">Namespaces the method is under, outermost first.</param>
/// <param name="OuterUsings">Using directives the source file declares outside its namespace.</param>
/// <param name="InnerUsings">Using directives the source file declares inside its namespace.</param>
/// <param name="IsNamespaceFileScoped">True if the namespace is file scoped.</param>
/// <param name="IsCSharp14Extension">True if the method is a member of a C# 14 extension block.</param>
/// <param name="Parents">Types the method is nested in, outermost first.</param>
internal sealed record MethodLocation(
    int Index,
    EquatableArray<string> Namespaces,
    EquatableArray<string> OuterUsings,
    EquatableArray<string> InnerUsings,
    bool IsNamespaceFileScoped,
    bool IsCSharp14Extension,
    EquatableArray<MethodParentDeclaration> Parents)
{
    /// <summary>
    /// Gets the namespaces and types the method is nested in, joined with dots.
    /// </summary>
    public string Scope => string.Join(".", Namespaces.Concat(Parents.Select(static p => p.ScopeName)));

    /// <summary>
    /// Locates a method.
    /// </summary>
    /// <param name="method">Declaration of the method.</param>
    /// <param name="symbol">Symbol of the method.</param>
    /// <param name="location">Where the method sits.</param>
    /// <param name="root">Node to rewrite: the method, or the extension block which holds it.</param>
    /// <returns>False when the method is not nested in a type which can be declared again as partial.</returns>
    public static bool TryCreate(
        MethodDeclarationSyntax method,
        IMethodSymbol symbol,
        [NotNullWhen(true)] out MethodLocation? location,
        [NotNullWhen(true)] out SyntaxNode? root)
    {
        location = null;
        root = null;

        var parents = ImmutableArray.CreateBuilder<MethodParentDeclaration>();
        SyntaxNode? node = method;
#if ROSLYN_5_0_OR_GREATER
        ExtensionBlockDeclarationSyntax? extensionParent = null;
#endif
        while (node.Parent is not null)
        {
            node = node.Parent;
#if ROSLYN_5_0_OR_GREATER
            if (node is ExtensionBlockDeclarationSyntax eds)
            {
                extensionParent = eds;
                continue;
            }
#endif

            MethodParentDeclaration? mpd = node switch
            {
                ClassDeclarationSyntax o => new(MethodParent.Class, o.Identifier, o.Modifiers, o.TypeParameterList),
                StructDeclarationSyntax o => new(MethodParent.Struct, o.Identifier, o.Modifiers, o.TypeParameterList),
                RecordDeclarationSyntax o => new(MethodParent.Record, o.Identifier, o.Modifiers, o.TypeParameterList, o.ClassOrStructKeyword),
                InterfaceDeclarationSyntax o => new(MethodParent.Interface, o.Identifier, o.Modifiers, o.TypeParameterList),
                _ => null,
            };

            if (mpd is null)
            {
                break;
            }

            parents.Insert(0, mpd);
        }

        if (parents.Count == 0)
        {
            return false;
        }

        var isNamespaceFileScoped = false;
        var namespaces = ImmutableArray.CreateBuilder<string>();

        // Documentation comments are copied across verbatim, and a cref in one is resolved
        // against the file it lands in rather than the file it was written in. Without the
        // using directives which were in scope where it was written, every cref which relied on
        // one goes unresolved, which a project building with warnings as errors reads as a
        // build failure. The directives are kept on the side of the namespace they were
        // declared on, since one declared inside a namespace may name it only relatively.
        var outerUsings = ImmutableArray.CreateBuilder<string>();
        var innerUsings = ImmutableArray.CreateBuilder<string>();

        while (node is not null and not CompilationUnitSyntax)
        {
            switch (node)
            {
                case NamespaceDeclarationSyntax nds:
                    namespaces.Insert(0, nds.Name.ToString());
                    InsertUsings(innerUsings, nds.Usings);
                    break;
                case FileScopedNamespaceDeclarationSyntax file:
                    namespaces.Add(file.Name.ToString());
                    InsertUsings(innerUsings, file.Usings);
                    isNamespaceFileScoped = true;
                    break;
                default:
                    throw new InvalidOperationException($"Cannot handle {node}");
            }

            node = node.Parent;
        }

        if (node is CompilationUnitSyntax compilationUnit)
        {
            InsertUsings(outerUsings, compilationUnit.Usings);
        }

#if ROSLYN_5_0_OR_GREATER
        var isCSharp14Extension = extensionParent is not null;
        root = extensionParent ?? (SyntaxNode)method;
#else
        var isCSharp14Extension = false;
        root = method;
#endif

        location = new(
            GetIndex(symbol),
            namespaces.ToImmutable(),
            outerUsings.ToImmutable(),
            innerUsings.ToImmutable(),
            isNamespaceFileScoped,
            isCSharp14Extension,
            parents.ToImmutable());

        return true;
    }

    /// <summary>
    /// Counts the members of the containing type which share the method's name and come before
    /// it, so that overloads are told apart in the names of the files they are emitted to.
    /// </summary>
    /// <param name="symbol">Symbol of the method.</param>
    /// <returns>Position of the method among its namesakes, starting at 1.</returns>
    private static int GetIndex(IMethodSymbol symbol)
    {
        var index = 1;

        if (symbol.ContainingType is { } containingType)
        {
            foreach (var member in containingType.GetMembers())
            {
                if (member.Equals(symbol, SymbolEqualityComparer.Default))
                {
                    break;
                }

                if (member.Name.Equals(symbol.Name, StringComparison.Ordinal))
                {
                    ++index;
                }
            }
        }

        return index;
    }

    /// <summary>
    /// Records the directives, innermost first, so that walking outwards from the method builds
    /// them up in the order they were written.
    /// </summary>
    /// <param name="destination">Collected directives.</param>
    /// <param name="usings">Directives declared at one level.</param>
    private static void InsertUsings(ImmutableArray<string>.Builder destination, SyntaxList<UsingDirectiveSyntax> usings)
    {
        var index = 0;
        foreach (var @using in usings)
        {
            destination.Insert(index++, @using.ToString());
        }
    }
}
