<#
.SYNOPSIS
    SIMPLE BUILD AND DEPLOY - One button does everything!

.DESCRIPTION
    1. Builds UnifiedUI (your main app)
    2. Copies to deploy folder
    3. Ready to use with SolidWorks
    
.EXAMPLE
    .\BUILD-AND-DEPLOY.ps1
    
    That's it! Just run this script.
#>

$ErrorActionPreference = "Stop"

Write-Host "`n========================================" -ForegroundColor Cyan
Write-Host "  ?? BUILD AND DEPLOY - SIMPLE MODE" -ForegroundColor Green
Write-Host "========================================`n" -ForegroundColor Cyan

# Paths
$msbuild = "C:\Program Files\Microsoft Visual Studio\2022\Professional\MSBuild\Current\Bin\MSBuild.exe"
$projectPath = "macros\csharp\Solidworks-Automation\UnifiedUI\UnifiedUI.csproj"
$buildOutput = "macros\csharp\Solidworks-Automation\UnifiedUI\bin\Release\net481"
$deployFolder = "deploy\UnifiedUI"

# STEP 1: Build UnifiedUI
Write-Host "?? STEP 1: Building UnifiedUI..." -ForegroundColor Yellow
Write-Host "   (This includes Bundle, Header, Hood, and all components)`n" -ForegroundColor Gray

& $msbuild $projectPath /p:Configuration=Release /v:minimal /nologo

if ($LASTEXITCODE -ne 0) {
    Write-Host "`n? BUILD FAILED!" -ForegroundColor Red
    Write-Host "   Check error messages above`n" -ForegroundColor White
    exit 1
}

Write-Host "`n? Build successful!`n" -ForegroundColor Green

# STEP 2: Create deploy folder
Write-Host "?? STEP 2: Preparing deploy folder..." -ForegroundColor Yellow

if (Test-Path $deployFolder) {
    Remove-Item $deployFolder -Recurse -Force
}
New-Item -ItemType Directory -Path $deployFolder -Force | Out-Null

Write-Host "   ? Folder created: $deployFolder`n" -ForegroundColor Green

# STEP 3: Copy files
Write-Host "?? STEP 3: Copying files to deploy..." -ForegroundColor Yellow

# Copy main exe and DLLs
Copy-Item "$buildOutput\UnifiedUI.exe" $deployFolder
Copy-Item "$buildOutput\*.dll" $deployFolder -Exclude "*vshost*","*CodeAnalysis*"
Copy-Item "$buildOutput\UnifiedUI.exe.config" $deployFolder -ErrorAction SilentlyContinue

# Count files
$fileCount = (Get-ChildItem $deployFolder -File).Count
Write-Host "   ? Copied $fileCount files`n" -ForegroundColor Green

# STEP 4: Done!
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "  ? DEPLOYMENT COMPLETE!" -ForegroundColor Green
Write-Host "========================================`n" -ForegroundColor Cyan

Write-Host "?? FILES READY HERE:" -ForegroundColor Yellow
Write-Host "   $((Get-Item $deployFolder).FullName)`n" -ForegroundColor Cyan

Write-Host "?? TO USE:" -ForegroundColor Yellow
Write-Host "   1. Open SolidWorks" -ForegroundColor White
Write-Host "   2. Run: $deployFolder\UnifiedUI.exe" -ForegroundColor Cyan
Write-Host "   3. Select your tool (Header, XCH, Z, or Hudson)" -ForegroundColor White
Write-Host "   4. Generate components!`n" -ForegroundColor White

Write-Host "?? TIP: Create desktop shortcut to UnifiedUI.exe`n" -ForegroundColor Gray

# Create a quick launch script
$launchScript = @"
# Quick Launch UnifiedUI
cd "$((Get-Item $deployFolder).FullName)"
.\UnifiedUI.exe
"@

$launchScript | Out-File "$deployFolder\LAUNCH.ps1" -Encoding UTF8
Write-Host "? Created LAUNCH.ps1 for quick start`n" -ForegroundColor Green

