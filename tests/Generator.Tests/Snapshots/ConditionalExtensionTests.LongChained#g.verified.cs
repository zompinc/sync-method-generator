//HintName: Test.Class.MethodAsync.g.cs
global::System.Reflection.Assembly? a = null;
_ = ((global::System.Func<global::System.Reflection.Assembly?,global::System.Collections.Generic.IEnumerable<global::System.Attribute>?>)((param)=>
{
    if((object?)param == null)
    {
        return null;
    }

    global::System.Collections.Generic.IEnumerable<global::System.Attribute> check0 =global::System.Reflection.CustomAttributeExtensions.GetCustomAttributes(param);
    if((object?)check0 == null)
    {
        return null;
    }

    global::System.Collections.Generic.List<global::System.Attribute> check1 =global::System.Linq.Enumerable.ToList(check0);
    if((object?)check1 == null)
    {
        return null;
    }

    global::System.Collections.Generic.IEnumerable<global::System.Attribute> check2 =global::System.Linq.Enumerable.Where(check1, z => 1 == 0);
    if((object?)check2 == null)
    {
        return null;
    }

    global::System.Collections.Generic.IEnumerable<global::System.Attribute> check3 =global::System.Linq.Enumerable.Where(check2, z => 2 == 0);
    return (object?)check3 == null?(global::System.Collections.Generic.IEnumerable<global::System.Attribute>?)null: global::System.Linq.Enumerable.Where(check3, z => 3 == 0);
}))(a);
