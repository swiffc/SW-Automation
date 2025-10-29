# PROJECT_HEALTH_CHECK.ps1
# Comprehensive automated health check for Solidworks Automation project
# Run from project root: .\scripts\utilities\PROJECT_HEALTH_CHECK.ps1

Write-Host "`n========================================" -ForegroundColor Cyan
Write-Host "   COMPREHENSIVE PROJECT HEALTH CHECK" -ForegroundColor Green
Write-Host "========================================`n" -ForegroundColor Cyan

$issues = @()
$warnings = @()

# 1. Build Check
Write-Host "[1/6] Checking build status..." -ForegroundColor Yellow
Push-Location "macros\csharp\Solidworks-Automation"
$buildResult = & "C:\Program Files\Microsoft Visual Studio\2022\Professional\MSBuild\Current\Bin\MSBuild.exe" `
  "Solidworks Automation.sln" /p:Configuration=Debug /v:q /nologo 2>&1 | Out-String
Pop-Location

if ($buildResult -match "Build FAILED|error") {
    $issues += "? Solution does not build"
    Write-Host "  ? Build FAILED" -ForegroundColor Red
} else {
    Write-Host "  ? Build succeeds" -ForegroundColor Green
}

# 2. File Organization
Write-Host "`n[2/6] Checking file organization..." -ForegroundColor Yellow
$allowedInRoot = @("README.md","requirements.txt","config.json","ROOT-Organize.ps1","LICENSE","AGENTS.md")
$misplaced = Get-ChildItem -Path . -File | Where-Object { 
    ($_.Name -notlike ".*") -and ($allowedInRoot -notcontains $_.Name)
}
if ($misplaced) {
    $issues += "? Files misplaced in root: $($misplaced.Name -join ', ')"
    Write-Host "  ? Root has misplaced files:" -ForegroundColor Red
    $misplaced | ForEach-Object { Write-Host "     - $($_.Name)" -ForegroundColor Red }
} else {
    Write-Host "  ? Root folder organized" -ForegroundColor Green
}

# 3. Critical Files
Write-Host "`n[3/6] Checking critical files..." -ForegroundColor Yellow
$criticalFiles = @(
    "macros\csharp\Solidworks-Automation\FileTools\Infrastructure\GlobalErrorHandler.cs",
    "macros\csharp\Solidworks-Automation\FileTools\Infrastructure\ComObjectManager.cs",
    "macros\csharp\Solidworks-Automation\FileTools\CommonData\CommonData.cs",
    "macros\csharp\Solidworks-Automation\Excel\Prego.cs",
    "macros\csharp\Solidworks-Automation\Excel\Header_DataManager.cs",
    "config.json",
    ".cursorrules"
)
$missing = $criticalFiles | Where-Object { -not (Test-Path $_) }
if ($missing) {
    $issues += "? Critical files missing: $($missing -join ', ')"
    Write-Host "  ? Missing critical files:" -ForegroundColor Red
    $missing | ForEach-Object { Write-Host "     - $_" -ForegroundColor Red }
} else {
    Write-Host "  ? All critical files present" -ForegroundColor Green
}

# 4. Error Handling Pattern
Write-Host "`n[4/6] Checking error handling..." -ForegroundColor Yellow
$formsWithoutErrorHandling = Get-ChildItem -Path "macros\csharp\Solidworks-Automation" -Recurse -Filter "*Form.cs" -ErrorAction SilentlyContinue | Where-Object {
    $content = Get-Content $_.FullName -Raw
    ($content -notmatch "GlobalErrorHandler") -and ($content -match "_Click")
}
if ($formsWithoutErrorHandling) {
    $warnings += "?? Some forms missing error handling: $($formsWithoutErrorHandling.Count) files"
    Write-Host "  ?? Warning: $($formsWithoutErrorHandling.Count) forms may lack error handling" -ForegroundColor Yellow
} else {
    Write-Host "  ? Error handling present in forms" -ForegroundColor Green
}

# 5. COM Safety
Write-Host "`n[5/6] Checking COM object safety..." -ForegroundColor Yellow
$csFiles = Get-ChildItem -Path "macros\csharp\Solidworks-Automation" -Recurse -Filter "*.cs" -ErrorAction SilentlyContinue
$comLeaks = $csFiles | Where-Object {
    $content = Get-Content $_.FullName -Raw -ErrorAction SilentlyContinue
    ($content -match "GetActiveObject.*SldWorks") -and 
    ($content -notmatch "ReleaseComObject") -and 
    ($content -notmatch "ComObjectManager")
}
if ($comLeaks) {
    $warnings += "?? Potential COM leaks detected in $($comLeaks.Count) files"
    Write-Host "  ?? Warning: Potential COM memory leaks in $($comLeaks.Count) files" -ForegroundColor Yellow
} else {
    Write-Host "  ? COM safety checks pass" -ForegroundColor Green
}

# 6. Hardcoded Paths
Write-Host "`n[6/6] Checking for hardcoded paths..." -ForegroundColor Yellow
$hardcodedPaths = $csFiles | Where-Object {
    $content = Get-Content $_.FullName -Raw -ErrorAction SilentlyContinue
    $content -match 'C:\\AXC_VAULT'
}
if ($hardcodedPaths) {
    $warnings += "?? Found hardcoded AXC_VAULT paths in $($hardcodedPaths.Count) files"
    Write-Host "  ?? Warning: Hardcoded production paths in $($hardcodedPaths.Count) files" -ForegroundColor Yellow
} else {
    Write-Host "  ? No hardcoded paths" -ForegroundColor Green
}

# Summary
Write-Host "`n========================================" -ForegroundColor Cyan
Write-Host "   HEALTH CHECK SUMMARY" -ForegroundColor Green
Write-Host "========================================" -ForegroundColor Cyan

if ($issues.Count -eq 0 -and $warnings.Count -eq 0) {
    Write-Host "`n? PROJECT IS HEALTHY!" -ForegroundColor Green
    Write-Host "`nAll checks passed. Project is ready for development." -ForegroundColor White
} elseif ($issues.Count -eq 0) {
    Write-Host "`n?? PROJECT IS FUNCTIONAL (WITH WARNINGS)" -ForegroundColor Yellow
    Write-Host "`nWarnings:" -ForegroundColor Yellow
    $warnings | ForEach-Object { Write-Host "  $_" -ForegroundColor Yellow }
} else {
    Write-Host "`n? PROJECT HAS ISSUES" -ForegroundColor Red
    Write-Host "`nCritical Issues:" -ForegroundColor Red
    $issues | ForEach-Object { Write-Host "  $_" -ForegroundColor Red }
    
    if ($warnings.Count -gt 0) {
        Write-Host "`nWarnings:" -ForegroundColor Yellow
        $warnings | ForEach-Object { Write-Host "  $_" -ForegroundColor Yellow }
    }
    
    Write-Host "`nPlease fix critical issues before making changes." -ForegroundColor Red
}

Write-Host "`nRecommended Actions:" -ForegroundColor Cyan
if ($misplaced) {
    Write-Host "  1. Run: .\ROOT-Organize.ps1" -ForegroundColor White
}
if ($buildResult -match "Build FAILED") {
    Write-Host "  2. Fix build errors in Visual Studio" -ForegroundColor White
}
Write-Host "  3. Review warnings and address if needed" -ForegroundColor White
Write-Host ""

# Exit with appropriate code
if ($issues.Count -gt 0) {
    exit 1
} else {
    exit 0
}

