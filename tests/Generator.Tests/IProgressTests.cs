namespace Generator.Tests;

public class IProgressTests
{
    [Theory]
    [InlineData("progress")]
    [InlineData("progress as IProgress<float>")]
    [InlineData("ProgressFunc(progress)")]
    [InlineData("(Progress<float>)progress")]
    [InlineData("(progress)")]
    [InlineData("someBool ? progress : null")]
    [InlineData("someBool ? null : progress")]
    [InlineData("classWithProgress.Property")]
    [InlineData("array[0]")]
    [InlineData("customProgress++")]
    [InlineData("customProgress + customProgress")]
    [InlineData("(Progress<float>)classWithProgress")]
    [InlineData("new CustomProgress()")]
    [InlineData("SomeMethod(progress)")]
    public Task DropIProgressExpressionArgument(string callArgument) => $$"""
public static async Task WithIProgress()
{
}

public static async Task WithIProgressAsync(IProgress<float>? progress = null)
{
}

static Func<IProgress<float>?, IProgress<float>?> ProgressFunc = (p) => p;

bool someBool = true;

class ClassWithProgress
{
    Progress<float> pg = new();
    public Progress<float> Property => pg;
    public static implicit operator Progress<float>(ClassWithProgress a) => a.pg;
}

class CustomProgress : IProgress<float>
{
    public static CustomProgress operator ++(CustomProgress a) => a;
    public static CustomProgress operator +(CustomProgress a, CustomProgress b) => a;
    public void Report(float value) => throw new NotImplementedException();
}

static CustomProgress customProgress = new();

static Progress<float>[] array = Array.Empty<Progress<float>>();

static ClassWithProgress classWithProgress = new();

static IProgress<T> SomeMethod<T>(IProgress<T> p) => p;

[CreateSyncVersion]
public static async Task CallWithIProgressAsync()
{
    var progress = new Progress<float>();
    await WithIProgressAsync({{callArgument}});
}
""".Verify(false, true);

    [Theory]
    [InlineData(1, "progress")]
    [InlineData(2, "progress as IProgress<float>")]
    [InlineData(3, "ProgressFunc(progress)")]
    [InlineData(5, "(Progress<float>)progress")]
    [InlineData(6, "(progress)")]
    [InlineData(7, "someBool ? progress : null")]
    [InlineData(8, "someBool ? null : progress")]
    [InlineData(9, "classWithProgress.Property")]
    [InlineData(10, "array[0]")]
    [InlineData(11, "customProgress++")]
    [InlineData(12, "customProgress + customProgress")]
    [InlineData(13, "(Progress<float>)classWithProgress")]
    [InlineData(14, "new CustomProgress()")]
    [InlineData(15, "SomeMethod(progress)")]
    public Task PreserveIProgressExpressionArgument(int n, string callArgument) => $$"""
public static void WithIProgress(IProgress<float>? progress = null)
{
}

public static async Task WithIProgressAsync(IProgress<float>? progress = null)
{
}

static Func<IProgress<float>?, IProgress<float>?> ProgressFunc = (p) => p;

bool someBool = true;

class ClassWithProgress
{
    Progress<float> pg = new();
    public Progress<float> Property => pg;
    public static implicit operator Progress<float>(ClassWithProgress a) => a.pg;
}

class CustomProgress : IProgress<float>
{
    public static CustomProgress operator ++(CustomProgress a) => a;
    public static CustomProgress operator +(CustomProgress a, CustomProgress b) => a;
    public void Report(float value) => throw new NotImplementedException();
}

static CustomProgress customProgress = new();

static Progress<float>[] array = Array.Empty<Progress<float>>();

static ClassWithProgress classWithProgress = new();

static IProgress<T> SomeMethod<T>(IProgress<T> p) => p;

[CreateSyncVersion(PreserveProgress = true)]
public static async Task CallWithIProgressAsync()
{
    var progress = new Progress<float>();
    await WithIProgressAsync({{callArgument}});
}
""".Verify(parameters: n.ToString("D2", NumberFormatInfo.InvariantInfo));

    [Theory]
    [InlineData("progress++;")]
    [InlineData("if (true) { progress++; }")]
    [InlineData("if (true) progress++;")]
    [InlineData("if (true) { } else progress++;")]
    [InlineData("if (false) { } else if (true) progress++;")]
    [InlineData("if (false) { } else if (true) progress++; else { }")]
    [InlineData("""
        switch (k)
        {
            case 1:
                progress++;
                break;
            default:
                progress++;
                progress++;
                break;
        }
        """)]

    public Task DropIProgressStatement(string statement) => $$"""
public static async Task WithIProgressAsync(IProgress<float>? progress = null)
{
    await Task.CompletedTask;
}

public static void WithIProgress()
{
}

static int k = 2;

[CreateSyncVersion]
public static async Task CallWithIProgressAsync()
{
    CustomProgress progress = new();

    {{statement}}

    await WithIProgressAsync(progress);
}

private sealed class CustomProgress : IProgress<float>
{
    public static CustomProgress operator ++(CustomProgress a) => a;

    public void Report(float value) => throw new NotImplementedException();
}
""".Verify(false, true);

    [Theory]
    [InlineData(1, "progress++;")]
    [InlineData(2, "if (true) { progress++; }")]
    [InlineData(3, "if (true) progress++;")]
    [InlineData(4, "if (true) { } else progress++;")]
    [InlineData(5, "if (false) { } else if (true) progress++;")]
    [InlineData(6, "if (false) { } else if (true) progress++; else { }")]
    [InlineData(7, """
        switch (k)
        {
            case 1:
                progress++;
                break;
            default:
                progress++;
                progress++;
                break;
        }
        """)]
    public Task PreserveIProgressStatement(int n, string statement) => $$"""
public static async Task WithIProgressAsync(IProgress<float>? progress = null)
{
    await Task.CompletedTask;
}

public static void WithIProgress()
{
}

static int k = 2;

[CreateSyncVersion(PreserveProgress = true)]
public static async Task CallWithIProgressAsync()
{
    CustomProgress progress = new();

    {{statement}}

    await WithIProgressAsync(progress);
}

private sealed class CustomProgress : IProgress<float>
{
    public static CustomProgress operator ++(CustomProgress a) => a;

    public void Report(float value) => throw new NotImplementedException();
}
""".Verify(parameters: n.ToString("D2", NumberFormatInfo.InvariantInfo));
}
