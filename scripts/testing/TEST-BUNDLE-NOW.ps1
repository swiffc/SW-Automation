# ?? BUNDLE ASSEMBLY - QUICK TEST LAUNCHER
# Run this script to test the Bundle assembly automation

Write-Host ""
Write-Host "=====================================" -ForegroundColor Cyan
Write-Host "  BUNDLE ASSEMBLY - QUICK TEST" -ForegroundColor Cyan
Write-Host "=====================================" -ForegroundColor Cyan
Write-Host ""

# Define paths
$bundleExe = "macros\csharp\Solidworks-Automation\Bundle\bin\Debug\Bundle.exe"
$logPath = "$env:APPDATA\BundleApp\Logs"

# Check if Bundle.exe exists
Write-Host "Checking if Bundle.exe exists..." -ForegroundColor Yellow
if (Test-Path $bundleExe) {
    Write-Host "? Bundle.exe found!" -ForegroundColor Green
} else {
    Write-Host "? Bundle.exe not found!" -ForegroundColor Red
    Write-Host ""
    Write-Host "Building the project first..." -ForegroundColor Yellow
    
    $msbuild = "C:\Program Files\Microsoft Visual Studio\2022\Professional\MSBuild\Current\Bin\MSBuild.exe"
    if (Test-Path $msbuild) {
        & $msbuild "macros\csharp\Solidworks-Automation\Bundle\Bundle.csproj" /p:Configuration=Debug /v:minimal
        
        if (Test-Path $bundleExe) {
            Write-Host "? Build successful!" -ForegroundColor Green
        } else {
            Write-Host "? Build failed. Check for errors above." -ForegroundColor Red
            exit 1
        }
    } else {
        Write-Host "? MSBuild not found. Install Visual Studio 2022." -ForegroundColor Red
        exit 1
    }
}

Write-Host ""

# Check for templates
Write-Host "Checking for templates..." -ForegroundColor Yellow
$templateCount = (Get-ChildItem "templates\hudson_certified\Bundle\" -File).Count
if ($templateCount -eq 21) {
    Write-Host "? All 21 templates found!" -ForegroundColor Green
} else {
    Write-Host "??  Warning: Expected 21 templates, found $templateCount" -ForegroundColor Yellow
}

Write-Host ""

# Check if SolidWorks is running
Write-Host "Checking for SolidWorks..." -ForegroundColor Yellow
$swProcess = Get-Process -Name "SLDWORKS" -ErrorAction SilentlyContinue
if ($swProcess) {
    Write-Host "? SolidWorks is running!" -ForegroundColor Green
    Write-Host "   You can test full bundle generation." -ForegroundColor Green
} else {
    Write-Host "??  SolidWorks is NOT running" -ForegroundColor Yellow
    Write-Host "   You can still test the UI and error handling." -ForegroundColor Yellow
    Write-Host "   (The app will show a retry dialog - this is expected!)" -ForegroundColor Cyan
}

Write-Host ""
Write-Host "=====================================" -ForegroundColor Cyan
Write-Host "  LAUNCHING BUNDLE.EXE..." -ForegroundColor Cyan
Write-Host "=====================================" -ForegroundColor Cyan
Write-Host ""

# Instructions
Write-Host "TEST CONFIGURATION:" -ForegroundColor Yellow
Write-Host "  Job Number: S25TEST" -ForegroundColor White
Write-Host "  Bundle Width: 48.500" -ForegroundColor White
Write-Host "  Side Frame THK: 0.375" -ForegroundColor White
Write-Host "  Side Frame Depth: 4.000" -ForegroundColor White
Write-Host "  Tube Length: 96.000" -ForegroundColor White
Write-Host "  Tube OD: 1.000" -ForegroundColor White
Write-Host "  Tube Row 1: 8" -ForegroundColor White
Write-Host "  Tube Row 2: 7" -ForegroundColor White
Write-Host ""

Write-Host "WHAT TO EXPECT:" -ForegroundColor Yellow
if ($swProcess) {
    Write-Host "  1. UI will open with input fields" -ForegroundColor White
    Write-Host "  2. Enter configuration values above" -ForegroundColor White
    Write-Host "  3. Click 'Create Bundle'" -ForegroundColor White
    Write-Host "  4. Watch SolidWorks generate 21 files!" -ForegroundColor White
    Write-Host "  5. Check output for S25TEST-7.SLDASM" -ForegroundColor White
} else {
    Write-Host "  1. UI will open with input fields" -ForegroundColor White
    Write-Host "  2. Try clicking 'Create Bundle'" -ForegroundColor White
    Write-Host "  3. You'll see a retry dialog (EXPECTED - not a crash!)" -ForegroundColor White
    Write-Host "  4. This proves error handling works" -ForegroundColor White
}

Write-Host ""
Write-Host "LOG FILES LOCATION:" -ForegroundColor Yellow
Write-Host "  $logPath" -ForegroundColor White
Write-Host ""

# Launch Bundle.exe
Write-Host "Starting Bundle.exe in 3 seconds..." -ForegroundColor Green
Start-Sleep -Seconds 3

try {
    Start-Process -FilePath $bundleExe -WorkingDirectory (Split-Path $bundleExe -Parent)
    
    Write-Host ""
    Write-Host "? Bundle.exe launched!" -ForegroundColor Green
    Write-Host ""
    Write-Host "After testing, check logs with:" -ForegroundColor Yellow
    Write-Host "  Start-Process '$logPath'" -ForegroundColor White
    Write-Host ""
    
    # Wait a bit then offer to open logs
    Start-Sleep -Seconds 5
    
    Write-Host "Would you like to open the log directory now? (Y/N)" -ForegroundColor Yellow
    $response = Read-Host
    if ($response -eq 'Y' -or $response -eq 'y') {
        Start-Process $logPath
    }
    
} catch {
    Write-Host ""
    Write-Host "? Error launching Bundle.exe:" -ForegroundColor Red
    Write-Host $_.Exception.Message -ForegroundColor Red
    Write-Host ""
    Write-Host "Try running as Administrator or check the build." -ForegroundColor Yellow
}

Write-Host ""
Write-Host "For detailed testing instructions, see:" -ForegroundColor Cyan
Write-Host "  BUNDLE_ASSEMBLY_TEST_GUIDE.md" -ForegroundColor White
Write-Host ""




