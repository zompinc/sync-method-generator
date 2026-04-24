param(
    [string]$configuration = "Release",
    [string]$verbosity = "minimal"
)

$artifacts = ".\packages"

function Exec
{
    [CmdletBinding()]
    param(
        [Parameter(Position=0,Mandatory=1)][scriptblock]$cmd,
        [Parameter(Position=1,Mandatory=0)][string]$errorMessage = "Error executing command $cmd"
    )
    & $cmd
    if ($lastexitcode -ne 0) {
        throw ("Exec: " + $errorMessage)
    }
}

if (Test-Path $artifacts) { Remove-Item $artifacts -Force -Recurse }
New-Item -Path $artifacts -ItemType Directory | Out-Null

$gen = "src/Zomp.SyncMethodGenerator/Zomp.SyncMethodGenerator.csproj"
$variants = @("roslyn4.8","roslyn4.12","roslyn5.0")

# Restore once, then build all variants with --no-restore
Write-Host "Restoring..." -ForegroundColor Cyan
Exec { & dotnet restore $gen --verbosity $verbosity }

Write-Host "Building Roslyn variants in parallel..." -ForegroundColor Cyan
$jobs = @()
foreach ($rv in $variants) {
    $jobs += Start-Process "dotnet" "build $gen -c $configuration -p RoslynVersion=$rv -p BaseIntermediateOutputPath=obj/$rv/ --verbosity $verbosity" -NoNewWindow -PassThru
}
$failed = $false
foreach ($job in $jobs) {
    $job.WaitForExit()
    if ($job.ExitCode -ne 0) { $failed = $true }
}
if ($failed) { throw "One or more variant builds failed" }

Write-Host "Packing..." -ForegroundColor Cyan
Exec { & dotnet pack src/Zomp.SyncMethodGenerator.Pack/Zomp.SyncMethodGenerator.Pack.csproj -c $configuration -o $artifacts --no-build --verbosity $verbosity }

Write-Host "Done! Package at $artifacts" -ForegroundColor Green
