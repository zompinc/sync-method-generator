namespace Zomp.SyncMethodGenerator;

//[N.SourceGenerators.UnionTypes.UnionType(typeof(List<StatementSyntax>), "NewStatements")]
//[N.SourceGenerators.UnionTypes.UnionType(typeof(bool), "RemoveLeadingEndIf")]
[System.Diagnostics.CodeAnalysis.SuppressMessage("StyleCop.CSharp.NamingRules", "SA1309:Field names should not begin with underscore", Justification = "issue https://github.com/Ne4to/N.SourceGenerators.UnionTypes/issues/34")]
[System.Diagnostics.CodeAnalysis.SuppressMessage("StyleCop.CSharp.OrderingRules", "SA1203:Constants should appear before fields", Justification = "issue https://github.com/Ne4to/N.SourceGenerators.UnionTypes/issues/34")]
[System.Diagnostics.CodeAnalysis.SuppressMessage("StyleCop.CSharp.LayoutRules", "SA1503:Braces should not be omitted", Justification = "issue https://github.com/Ne4to/N.SourceGenerators.UnionTypes/issues/34")]
[System.Diagnostics.CodeAnalysis.SuppressMessage("StyleCop.CSharp.LayoutRules", "SA1516:Elements should be separated by blank line", Justification = "issue https://github.com/Ne4to/N.SourceGenerators.UnionTypes/issues/34")]
[System.Diagnostics.CodeAnalysis.SuppressMessage("StyleCop.CSharp.OrderingRules", "SA1201:Elements should appear in the correct order", Justification = "issue https://github.com/Ne4to/N.SourceGenerators.UnionTypes/issues/34")]
internal sealed partial class Operation
{
    private readonly int _variantId;
    private const int NewStatementsId = 1;
    private readonly global::System.Collections.Generic.List<global::Microsoft.CodeAnalysis.CSharp.Syntax.StatementSyntax> _newStatements;
    public bool IsNewStatements => _variantId == NewStatementsId;

    public global::System.Collections.Generic.List<global::Microsoft.CodeAnalysis.CSharp.Syntax.StatementSyntax> AsNewStatements
    {
        get
        {
            if (_variantId == NewStatementsId)
                return _newStatements;
            throw new System.InvalidOperationException($"Unable convert to NewStatements. Inner value is {ValueAlias} not NewStatements.");
        }
    }

    public Operation(global::System.Collections.Generic.List<global::Microsoft.CodeAnalysis.CSharp.Syntax.StatementSyntax> newStatements)
    {
        _variantId = NewStatementsId;
        _newStatements = newStatements;
    }

    public static implicit operator Operation(global::System.Collections.Generic.List<global::Microsoft.CodeAnalysis.CSharp.Syntax.StatementSyntax> newStatements) => new Operation(newStatements);
    public static explicit operator global::System.Collections.Generic.List<global::Microsoft.CodeAnalysis.CSharp.Syntax.StatementSyntax>(Operation value)
    {
        if (value._variantId == NewStatementsId)
            return value._newStatements;
        throw new System.InvalidOperationException($"Unable convert to NewStatements. Inner value is {value.ValueAlias} not NewStatements.");
    }

    public bool TryGetNewStatements([System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out global::System.Collections.Generic.List<global::Microsoft.CodeAnalysis.CSharp.Syntax.StatementSyntax> value)
    {
        if (_variantId == NewStatementsId)
        {
            value = _newStatements;
            return true;
        }
        else
        {
            value = [];
            return false;
        }
    }

    private const int RemoveLeadingEndIfId = 2;
    private readonly bool _removeLeadingEndIf;
    public bool IsRemoveLeadingEndIf => _variantId == RemoveLeadingEndIfId;

    public bool AsRemoveLeadingEndIf
    {
        get
        {
            if (_variantId == RemoveLeadingEndIfId)
                return _removeLeadingEndIf;
            throw new System.InvalidOperationException($"Unable convert to RemoveLeadingEndIf. Inner value is {ValueAlias} not RemoveLeadingEndIf.");
        }
    }

    public Operation(bool removeLeadingEndIf)
    {
        _variantId = RemoveLeadingEndIfId;
        _removeLeadingEndIf = removeLeadingEndIf;
        _newStatements = [];
    }

    public static implicit operator Operation(bool removeLeadingEndIf) => new Operation(removeLeadingEndIf);
    public static explicit operator bool(Operation value)
    {
        if (value._variantId == RemoveLeadingEndIfId)
            return value._removeLeadingEndIf;
        throw new System.InvalidOperationException($"Unable convert to RemoveLeadingEndIf. Inner value is {value.ValueAlias} not RemoveLeadingEndIf.");
    }

    public bool TryGetRemoveLeadingEndIf([System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out bool value)
    {
        if (_variantId == RemoveLeadingEndIfId)
        {
            value = _removeLeadingEndIf;
            return true;
        }
        else
        {
            value = default;
            return false;
        }
    }

    public TOut Match<TOut>(global::System.Func<global::System.Collections.Generic.List<global::Microsoft.CodeAnalysis.CSharp.Syntax.StatementSyntax>, TOut> matchNewStatements, global::System.Func<bool, TOut> matchRemoveLeadingEndIf)
    {
        if (_variantId == NewStatementsId)
            return matchNewStatements(_newStatements);
        if (_variantId == RemoveLeadingEndIfId)
            return matchRemoveLeadingEndIf(_removeLeadingEndIf);
        throw new System.InvalidOperationException("Inner type is unknown");
    }

    public async global::System.Threading.Tasks.Task<TOut> MatchAsync<TOut>(global::System.Func<global::System.Collections.Generic.List<global::Microsoft.CodeAnalysis.CSharp.Syntax.StatementSyntax>, global::System.Threading.CancellationToken, global::System.Threading.Tasks.Task<TOut>> matchNewStatements, global::System.Func<bool, global::System.Threading.CancellationToken, global::System.Threading.Tasks.Task<TOut>> matchRemoveLeadingEndIf, global::System.Threading.CancellationToken ct)
    {
        if (_variantId == NewStatementsId)
            return await matchNewStatements(_newStatements, ct).ConfigureAwait(false);
        if (_variantId == RemoveLeadingEndIfId)
            return await matchRemoveLeadingEndIf(_removeLeadingEndIf, ct).ConfigureAwait(false);
        throw new System.InvalidOperationException("Inner type is unknown");
    }

    public void Switch(global::System.Action<global::System.Collections.Generic.List<global::Microsoft.CodeAnalysis.CSharp.Syntax.StatementSyntax>> switchNewStatements, global::System.Action<bool> switchRemoveLeadingEndIf)
    {
        if (_variantId == NewStatementsId)
        {
            switchNewStatements(_newStatements);
            return;
        }

        if (_variantId == RemoveLeadingEndIfId)
        {
            switchRemoveLeadingEndIf(_removeLeadingEndIf);
            return;
        }

        throw new System.InvalidOperationException("Inner type is unknown");
    }

    public async global::System.Threading.Tasks.Task SwitchAsync(global::System.Func<global::System.Collections.Generic.List<global::Microsoft.CodeAnalysis.CSharp.Syntax.StatementSyntax>, global::System.Threading.CancellationToken, global::System.Threading.Tasks.Task> switchNewStatements, global::System.Func<bool, global::System.Threading.CancellationToken, global::System.Threading.Tasks.Task> switchRemoveLeadingEndIf, global::System.Threading.CancellationToken ct)
    {
        if (_variantId == NewStatementsId)
        {
            await switchNewStatements(_newStatements, ct).ConfigureAwait(false);
            return;
        }

        if (_variantId == RemoveLeadingEndIfId)
        {
            await switchRemoveLeadingEndIf(_removeLeadingEndIf, ct).ConfigureAwait(false);
            return;
        }

        throw new System.InvalidOperationException("Inner type is unknown");
    }

    public global::System.Type ValueType
    {
        get
        {
            if (_variantId == NewStatementsId)
                return typeof(global::System.Collections.Generic.List<global::Microsoft.CodeAnalysis.CSharp.Syntax.StatementSyntax>);
            if (_variantId == RemoveLeadingEndIfId)
                return typeof(bool);
            throw new System.InvalidOperationException("Inner type is unknown");
        }
    }

    private string ValueAlias
    {
        get
        {
            if (_variantId == NewStatementsId)
                return "NewStatements";
            if (_variantId == RemoveLeadingEndIfId)
                return "RemoveLeadingEndIf";
            throw new System.InvalidOperationException("Inner type is unknown");
        }
    }

    public override int GetHashCode()
    {
        if (_variantId == NewStatementsId)
            return _newStatements.GetHashCode();
        if (_variantId == RemoveLeadingEndIfId)
            return _removeLeadingEndIf.GetHashCode();
        throw new System.InvalidOperationException("Inner type is unknown");
    }

    public static bool operator ==(Operation? left, Operation? right)
    {
        return Equals(left, right);
    }

    public static bool operator !=(Operation? left, Operation? right)
    {
        return !Equals(left, right);
    }

    public bool Equals(Operation? other)
    {
        if (ReferenceEquals(null, other))
        {
            return false;
        }

        if (ReferenceEquals(this, other))
        {
            return true;
        }

        if (ValueType != other.ValueType)
        {
            return false;
        }

        if (_variantId == NewStatementsId)
            return System.Collections.Generic.EqualityComparer<global::System.Collections.Generic.List<global::Microsoft.CodeAnalysis.CSharp.Syntax.StatementSyntax>>.Default.Equals(_newStatements, other._newStatements);
        if (_variantId == RemoveLeadingEndIfId)
            return System.Collections.Generic.EqualityComparer<bool>.Default.Equals(_removeLeadingEndIf, other._removeLeadingEndIf);
        throw new System.InvalidOperationException("Inner type is unknown");
    }

    public override string ToString()
    {
        if (_variantId == NewStatementsId)
            return _newStatements.ToString();
        if (_variantId == RemoveLeadingEndIfId)
            return _removeLeadingEndIf.ToString();
        throw new System.InvalidOperationException("Inner type is unknown");
    }

    public override bool Equals(object? other)
    {
        if (ReferenceEquals(null, other))
        {
            return false;
        }

        if (ReferenceEquals(this, other))
        {
            return true;
        }

        if (other.GetType() != typeof(Operation))
        {
            return false;
        }

        return Equals((Operation)other);
    }
}
