//HintName: Test.Class.MethodAsync.g.cs
global::System.Reflection.Assembly? a = null;
_ = ((global::System.Func<global::System.Reflection.Assembly?,global::System.Collections.Generic.List<global::System.Attribute>?>)((param)=>
{
    if((object?)param == null)
    {
        return null;
    }

    global::System.Collections.Generic.IEnumerable<global::System.Attribute> check0 =global::System.Reflection.CustomAttributeExtensions.GetCustomAttributes(param);
    return (object?)check0 == null?(global::System.Collections.Generic.List<global::System.Attribute>?)null: global::System.Linq.Enumerable.ToList(check0);
}))(a);
