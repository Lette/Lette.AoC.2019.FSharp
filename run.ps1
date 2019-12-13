$ErrorActionPreference = "Stop"
dotnet build -c Release
if ($LastExitCode -ne 0) { throw "build failed" }
$sw = [System.Diagnostics.Stopwatch]::StartNew()
dotnet run -c Release --no-build --project .\Aoc\
$ec = $LastExitCode
$e = $sw.ElapsedMilliseconds
if ($ec -ne 0) { throw "run failed" }
Write-Output "Run time: $e ms"
