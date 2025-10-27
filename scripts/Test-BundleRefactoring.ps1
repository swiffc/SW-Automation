# ?? QUICK TEST SCRIPT
# Run this script to quickly verify the refactoring works

Write-Host "?????????????????????????????????????????????????????????????" -ForegroundColor Cyan
Write-Host "?         BUNDLE REFACTORING - QUICK TEST SUITE        ?" -ForegroundColor Cyan
Write-Host "?????????????????????????????????????????????????????????????" -ForegroundColor Cyan
Write-Host ""

# Configuration
$bundlePath = "C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation\Bundle\bin\Debug\Bundle.exe"
$logPath = "$env:APPDATA\BundleApp\Logs"

# Test 1: Check if Bundle.exe exists
Write-Host "Test 1: Build Verification" -ForegroundColor Yellow
if (Test-Path $bundlePath) {
    Write-Host "  ? PASS - Bundle.exe found" -ForegroundColor Green
  $build Verified = $true
} else {
    Write-Host "  ? FAIL - Bundle.exe not found at: $bundlePath" -ForegroundColor Red
    Write-Host "  Action: Build the solution first (Ctrl+Shift+B in Visual Studio)" -ForegroundColor Yellow
    $buildVerified = $false
}
Write-Host ""

if (-not $buildVerified) {
    Write-Host "Cannot continue tests without build. Please build first." -ForegroundColor Red
    exit 1
}

# Test 2: Check log directory
Write-Host "Test 2: Log Directory Check" -ForegroundColor Yellow
if (Test-Path $logPath) {
    Write-Host "  ? PASS - Log directory exists: $logPath" -ForegroundColor Green
    $existingLogs = Get-ChildItem $logPath -Filter "*.log" | Sort-Object LastWriteTime -Descending
    if ($existingLogs) {
  Write-Host "  ??  Found $($existingLogs.Count) existing log file(s)" -ForegroundColor Cyan
        Write-Host "  Most recent: $($existingLogs[0].Name)" -ForegroundColor Cyan
    }
} else {
    Write-Host "  ??  Log directory doesn't exist yet (will be created on first run)" -ForegroundColor Yellow
}
Write-Host ""

# Test 3: Check if SolidWorks is running
Write-Host "Test 3: SolidWorks Status Check" -ForegroundColor Yellow
$swProcess = Get-Process -Name "SLDWORKS" -ErrorAction SilentlyContinue
if ($swProcess) {
    Write-Host "  ? SolidWorks IS running (PID: $($swProcess.Id))" -ForegroundColor Green
Write-Host "  Test Mode: Will test normal operation" -ForegroundColor Cyan
    $swRunning = $true
} else {
    Write-Host "  ??  SolidWorks is NOT running" -ForegroundColor Cyan
    Write-Host "  Test Mode: Will test graceful handling" -ForegroundColor Cyan
    $swRunning = $false
}
Write-Host ""

# Test 4: Check if Excel is running
Write-Host "Test 4: Excel Status Check" -ForegroundColor Yellow
$excelProcess = Get-Process -Name "EXCEL" -ErrorAction SilentlyContinue
if ($excelProcess) {
    Write-Host "  ? Excel IS running (PID: $($excelProcess.Id))" -ForegroundColor Green
    $excelRunning = $true
} else {
    Write-Host "  ??  Excel is NOT running" -ForegroundColor Cyan
    $excelRunning = $false
}
Write-Host ""

# Test 5: File integrity check
Write-Host "Test 5: Source File Integrity" -ForegroundColor Yellow
$criticalFiles = @(
 "C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation\FileTools\Infrastructure\GlobalErrorHandler.cs",
    "C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation\FileTools\Infrastructure\ComObjectManager.cs",
    "C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation\Bundle\Bundle.cs",
    "C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation\Bundle\BundleUI.cs",
    "C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation\Excel\Prego.cs"
)

$allFilesExist = $true
foreach ($file in $criticalFiles) {
  if (Test-Path $file) {
        $fileName = Split-Path $file -Leaf
        Write-Host "  ? $fileName exists" -ForegroundColor Green
    } else {
        Write-Host "  ? $fileName MISSING" -ForegroundColor Red
        $allFilesExist = $false
    }
}

if ($allFilesExist) {
    Write-Host "  ? PASS - All critical files present" -ForegroundColor Green
} else {
    Write-Host "  ? FAIL - Some files missing" -ForegroundColor Red
}
Write-Host ""

# Summary and next steps
Write-Host "?????????????????????????????????????????????????????????????" -ForegroundColor Cyan
Write-Host "?       TEST SUMMARY              ?" -ForegroundColor Cyan
Write-Host "?????????????????????????????????????????????????????????????" -ForegroundColor Cyan
Write-Host ""

if ($buildVerified -and $allFilesExist) {
    Write-Host "? READY FOR MANUAL TESTING!" -ForegroundColor Green
    Write-Host ""
    Write-Host "NEXT STEPS:" -ForegroundColor Yellow
 Write-Host ""
    Write-Host "1. LAUNCH BUNDLE:" -ForegroundColor White
    Write-Host "   Start-Process '$bundlePath'" -ForegroundColor Gray
    Write-Host ""
 
  if (-not $swRunning) {
        Write-Host "2. TEST WITHOUT SOLIDWORKS:" -ForegroundColor White
        Write-Host "   - Click 'Create Bundle' button" -ForegroundColor Gray
        Write-Host "   - Should show: 'SolidWorks is not currently running...'" -ForegroundColor Gray
        Write-Host "   - Verify 'Retry' and 'Cancel' buttons appear" -ForegroundColor Gray
      Write-Host ""
        Write-Host "3. TEST WITH SOLIDWORKS:" -ForegroundColor White
        Write-Host "   - Start SolidWorks" -ForegroundColor Gray
     Write-Host "   - Click 'Retry' in the dialog" -ForegroundColor Gray
        Write-Host "   - Should proceed normally" -ForegroundColor Gray
    } else {
        Write-Host "2. TEST WITH SOLIDWORKS (already running):" -ForegroundColor White
        Write-Host "   - Click 'Create Bundle' button" -ForegroundColor Gray
     Write-Host "   - Should proceed normally (no error)" -ForegroundColor Gray
    }
    Write-Host ""
    
    if (-not $excelRunning) {
        Write-Host "4. TEST EXCEL INTEGRATION:" -ForegroundColor White
        Write-Host "   - Click 'Import Prego' button" -ForegroundColor Gray
        Write-Host "   - Excel should start automatically" -ForegroundColor Gray
        Write-Host "   - No crash or error" -ForegroundColor Gray
    }
    Write-Host ""
    
    Write-Host "5. CHECK LOG FILE:" -ForegroundColor White
    Write-Host "   - After testing, open: $logPath" -ForegroundColor Gray
    Write-Host "   - Verify all operations logged" -ForegroundColor Gray
    Write-Host ""
    
    Write-Host "6. VERIFY NO MEMORY LEAKS:" -ForegroundColor White
    Write-Host "   - Open Task Manager" -ForegroundColor Gray
    Write-Host "   - Look for zombie EXCEL.EXE processes" -ForegroundColor Gray
    Write-Host "   - Should be clean after closing Bundle" -ForegroundColor Gray
    Write-Host ""
    
    # Offer to launch
    Write-Host "???????????????????????????????????????????????????????????" -ForegroundColor Cyan
    $launch = Read-Host "Launch Bundle.exe now? (Y/N)"
    if ($launch -eq 'Y' -or $launch -eq 'y') {
        Write-Host ""
      Write-Host "Launching Bundle.exe..." -ForegroundColor Green
        Start-Process $bundlePath
        Write-Host ""
        Write-Host "? Bundle.exe started!" -ForegroundColor Green
        Write-Host "Follow the test steps above." -ForegroundColor Yellow
     Write-Host ""
   Write-Host "Press any key to open log directory..." -ForegroundColor Yellow
        $null = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
        if (Test-Path $logPath) {
            Start-Process $logPath
        } else {
            Write-Host "Log directory doesn't exist yet. Run some operations first." -ForegroundColor Yellow
     }
    } else {
        Write-Host ""
        Write-Host "You can launch manually with:" -ForegroundColor Yellow
        Write-Host "  Start-Process '$bundlePath'" -ForegroundColor Gray
    }
} else {
    Write-Host "? NOT READY - Fix issues above first" -ForegroundColor Red
}

Write-Host ""
Write-Host "???????????????????????????????????????????????????????????" -ForegroundColor Cyan
Write-Host "Test script complete." -ForegroundColor White
Write-Host ""
