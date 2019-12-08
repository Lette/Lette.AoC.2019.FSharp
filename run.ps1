dotnet build -c Release
$sw = [System.Diagnostics.Stopwatch]::StartNew()
dotnet run -c Release --no-build --project .\Aoc\
$e = $sw.ElapsedMilliseconds
Write-Output "Run time: $e ms"
