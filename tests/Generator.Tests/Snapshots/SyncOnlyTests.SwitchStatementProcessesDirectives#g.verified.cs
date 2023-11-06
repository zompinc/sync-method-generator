//HintName: Test.Class.MethodAsync.g.cs
var i = 0;
switch (i)
{
    case 0:
        break;
    case 2:
        System.Console.Write("Sync");
        break;
    case 3:
#if SYMBOL
        System.Console.Write("Symbol");
#endif
        break;
}
