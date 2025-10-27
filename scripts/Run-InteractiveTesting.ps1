# ?? INTERACTIVE TESTING ASSISTANT

Write-Host "?????????????????????????????????????????????????????????????" -ForegroundColor Cyan
Write-Host "?    BUNDLE REFACTORING - INTERACTIVE TEST GUIDE    ?" -ForegroundColor Cyan
Write-Host "?????????????????????????????????????????????????????????????" -ForegroundColor Cyan
Write-Host ""

# Helper function to display test steps
function Show-TestStep {
    param(
        [string]$StepNumber,
  [string]$Title,
        [string]$Description,
        [string[]]$Instructions,
        [string[]]$ExpectedResults
 )
    
    Write-Host "???????????????????????????????????????????????????????????" -ForegroundColor Yellow
    Write-Host "? TEST $StepNumber : $Title" -ForegroundColor Yellow -NoNewline
    Write-Host (" " * (54 - $Title.Length - $StepNumber.Length)) -NoNewline
    Write-Host "?" -ForegroundColor Yellow
    Write-Host "???????????????????????????????????????????????????????????" -ForegroundColor Yellow
    Write-Host ""
    Write-Host "?? Description:" -ForegroundColor Cyan
    Write-Host "  $Description" -ForegroundColor White
    Write-Host ""

    Write-Host "?? Instructions:" -ForegroundColor Cyan
    $i = 1
    foreach ($instruction in $Instructions) {
        Write-Host "  $i. $instruction" -ForegroundColor White
    $i++
    }
    Write-Host ""
  
    Write-Host "? Expected Results:" -ForegroundColor Cyan
    foreach ($result in $ExpectedResults) {
        Write-Host "  • $result" -ForegroundColor Green
    }
    Write-Host ""
}

function Get-TestResult {
    Write-Host "???????????????????????????????????????????????????????????" -ForegroundColor Gray
    $result = Read-Host "Did the test PASS? (Y/N/S for Skip)"
    Write-Host ""
    
    switch ($result.ToUpper()) {
        'Y' { 
        Write-Host "? TEST PASSED" -ForegroundColor Green
            Write-Host ""
         return "PASS"
        }
        'N' {
      Write-Host "? TEST FAILED" -ForegroundColor Red
            $issue = Read-Host "Describe the issue"
            Write-Host ""
          Write-Host "Issue noted: $issue" -ForegroundColor Yellow
      Write-Host ""
            return "FAIL: $issue"
        }
        'S' {
            Write-Host "??  TEST SKIPPED" -ForegroundColor Yellow
    Write-Host ""
    return "SKIP"
        }
        default {
            Write-Host "Invalid input. Marking as SKIP" -ForegroundColor Yellow
            return "SKIP"
        }
    }
}

# Initialize test results
$testResults = @()
$passCount = 0
$failCount = 0
$skipCount = 0

# Welcome
Write-Host "Welcome to the Interactive Testing Assistant!" -ForegroundColor Green
Write-Host "This will guide you through testing the refactored code." -ForegroundColor White
Write-Host ""
Write-Host "Press ENTER to start..." -ForegroundColor Yellow
$null = Read-Host
Clear-Host

#???????????????????????????????????????????????????????????
# PHASE 1: BUILD VERIFICATION
#???????????????????????????????????????????????????????????

Write-Host ""
Write-Host "???????????????????????????????????????????????????????????" -ForegroundColor Cyan
Write-Host "           PHASE 1: BUILD VERIFICATION        " -ForegroundColor Cyan
Write-Host "???????????????????????????????????????????????????????????" -ForegroundColor Cyan
Write-Host ""

# Test 1.1: Check if build exists
Show-TestStep -StepNumber "1.1" -Title "Build Existence Check" `
    -Description "Verify Bundle.exe was built successfully" `
    -Instructions @(
        "Check if Bundle.exe exists in bin\Debug folder"
    ) `
    -ExpectedResults @(
        "File exists at: Bundle\bin\Debug\Bundle.exe"
    )

$bundlePath = "Bundle\bin\Debug\Bundle.exe"
if (Test-Path $bundlePath) {
    Write-Host "? AUTO-CHECK: Bundle.exe found!" -ForegroundColor Green
  Write-Host "   Path: $bundlePath" -ForegroundColor Gray
    $result = "PASS"
} else {
    Write-Host "? AUTO-CHECK: Bundle.exe NOT found!" -ForegroundColor Red
    Write-Host "   Expected path: $bundlePath" -ForegroundColor Gray
    Write-Host ""
    Write-Host "ACTION REQUIRED:" -ForegroundColor Yellow
 Write-Host "1. Open Visual Studio" -ForegroundColor White
    Write-Host "2. Press Ctrl+Shift+B to rebuild" -ForegroundColor White
    Write-Host "3. Fix any errors" -ForegroundColor White
    Write-Host "4. Run this test again" -ForegroundColor White
    $result = Get-TestResult
}
$testResults += "Test 1.1: $result"
if ($result -eq "PASS") { $passCount++ } 
elseif ($result -like "FAIL*") { $failCount++ }
else { $skipCount++ }

Write-Host "Press ENTER to continue..." -ForegroundColor Yellow
$null = Read-Host
Clear-Host

#???????????????????????????????????????????????????????????
# PHASE 2: QUICK FUNCTIONAL TEST
#???????????????????????????????????????????????????????????

Write-Host ""
Write-Host "???????????????????????????????????????????????????????????" -ForegroundColor Cyan
Write-Host "           PHASE 2: FUNCTIONAL TESTING       " -ForegroundColor Cyan
Write-Host "???????????????????????????????????????????????????????????" -ForegroundColor Cyan
Write-Host ""

# Test 2.1: Application Startup
Show-TestStep -StepNumber "2.1" -Title "Application Startup" `
    -Description "Verify Bundle.exe starts without crashing" `
    -Instructions @(
      "Double-click Bundle.exe (or run from this script)",
        "Wait for the UI to load",
        "Verify the form appears"
    ) `
    -ExpectedResults @(
  "Application window opens",
        "No error messages appear",
        "All controls are visible"
    )

Write-Host "Would you like to launch Bundle.exe now? (Y/N)" -ForegroundColor Yellow
$launch = Read-Host
if ($launch -eq 'Y' -or $launch -eq 'y') {
    Write-Host "Launching Bundle.exe..." -ForegroundColor Green
    if (Test-Path $bundlePath) {
        Start-Process $bundlePath
        Start-Sleep -Seconds 2
    Write-Host "? Bundle.exe launched!" -ForegroundColor Green
    } else {
        Write-Host "? Cannot launch - file not found" -ForegroundColor Red
    }
}

$result = Get-TestResult
$testResults += "Test 2.1: $result"
if ($result -eq "PASS") { $passCount++ } 
elseif ($result -like "FAIL*") { $failCount++ }
else { $skipCount++ }

Write-Host "Press ENTER to continue..." -ForegroundColor Yellow
$null = Read-Host
Clear-Host

# Test 2.2: SolidWorks Check (without SW running)
Write-Host ""
Show-TestStep -StepNumber "2.2" -Title "SolidWorks NOT Running Test" `
    -Description "Verify graceful handling when SolidWorks is not available" `
    -Instructions @(
        "Ensure SolidWorks is NOT running (check Task Manager)",
        "In Bundle.exe, click 'Create Bundle' button",
        "Observe the dialog that appears"
    ) `
    -ExpectedResults @(
        "Dialog appears with message about SolidWorks not running",
        "Dialog shows 'Retry' and 'Cancel' buttons",
        "Application does NOT crash",
        "Warning icon (??) is displayed"
    )

# Auto-check if SW is running
$swProcess = Get-Process -Name "SLDWORKS" -ErrorAction SilentlyContinue
if ($swProcess) {
    Write-Host "??  AUTO-CHECK: SolidWorks IS currently running (PID: $($swProcess.Id))" -ForegroundColor Yellow
    Write-Host "   Please close SolidWorks to test this scenario properly" -ForegroundColor White
 Write-Host ""
} else {
    Write-Host "? AUTO-CHECK: SolidWorks is NOT running - perfect for this test!" -ForegroundColor Green
    Write-Host ""
}

$result = Get-TestResult
$testResults += "Test 2.2: $result"
if ($result -eq "PASS") { $passCount++ } 
elseif ($result -like "FAIL*") { $failCount++ }
else { $skipCount++ }

Write-Host "Press ENTER to continue..." -ForegroundColor Yellow
$null = Read-Host
Clear-Host

# Test 2.3: Retry Functionality
Write-Host ""
Show-TestStep -StepNumber "2.3" -Title "Retry Functionality" `
    -Description "Test the retry mechanism for SolidWorks connection" `
    -Instructions @(
        "With the 'SolidWorks Required' dialog still open:",
        "Start SolidWorks (wait for it to fully load)",
        "Click 'Retry' button in the dialog",
 "Observe what happens"
    ) `
    -ExpectedResults @(
  "If SW now running: Process continues normally",
        "If SW still not ready: Shows 'Still unable to connect' message",
        "No crash in either case"
    )

$result = Get-TestResult
$testResults += "Test 2.3: $result"
if ($result -eq "PASS") { $passCount++ } 
elseif ($result -like "FAIL*") { $failCount++ }
else { $skipCount++ }

Write-Host "Press ENTER to continue..." -ForegroundColor Yellow
$null = Read-Host
Clear-Host

# Test 2.4: Log File Check
Write-Host ""
Show-TestStep -StepNumber "2.4" -Title "Log File Verification" `
    -Description "Verify logging system works correctly" `
    -Instructions @(
        "Navigate to: %AppData%\BundleApp\Logs\",
        "Find the most recent .log file",
        "Open it in Notepad",
        "Review the contents"
    ) `
    -ExpectedResults @(
        "Log file exists",
    "Contains timestamped entries",
        "Shows 'Application Started' message",
 "Shows operation logs (button clicks, etc.)",
        "Easy to read and understand"
    )

$logPath = "$env:APPDATA\BundleApp\Logs"
Write-Host "Log directory path: $logPath" -ForegroundColor Cyan
Write-Host ""

if (Test-Path $logPath) {
    Write-Host "? AUTO-CHECK: Log directory exists!" -ForegroundColor Green
    $logFiles = Get-ChildItem $logPath -Filter "*.log" | Sort-Object LastWriteTime -Descending
    if ($logFiles) {
      Write-Host "   Found $($logFiles.Count) log file(s)" -ForegroundColor Gray
        Write-Host "   Most recent: $($logFiles[0].Name)" -ForegroundColor Gray
        Write-Host ""
        Write-Host "Would you like to open the log directory? (Y/N)" -ForegroundColor Yellow
        $openLogs = Read-Host
        if ($openLogs -eq 'Y' -or $openLogs -eq 'y') {
            Start-Process $logPath
Write-Host "? Log directory opened!" -ForegroundColor Green
        }
    } else {
     Write-Host "??  No log files found yet" -ForegroundColor Yellow
        Write-Host "   This is normal if you haven't run Bundle.exe yet" -ForegroundColor Gray
    }
} else {
    Write-Host "??  AUTO-CHECK: Log directory doesn't exist yet" -ForegroundColor Yellow
    Write-Host "   Will be created on first run of Bundle.exe" -ForegroundColor Gray
}
Write-Host ""

$result = Get-TestResult
$testResults += "Test 2.4: $result"
if ($result -eq "PASS") { $passCount++ } 
elseif ($result -like "FAIL*") { $failCount++ }
else { $skipCount++ }

Write-Host "Press ENTER to continue..." -ForegroundColor Yellow
$null = Read-Host
Clear-Host

# Test 2.5: Excel Integration (optional)
Write-Host ""
Show-TestStep -StepNumber "2.5" -Title "Excel Integration Test" `
    -Description "Verify Excel COM handling works safely" `
    -Instructions @(
        "Ensure Excel is NOT running",
        "In Bundle.exe, click 'Import Prego' button",
        "Observe what happens"
    ) `
    -ExpectedResults @(
   "Excel starts automatically (or uses existing instance)",
        "No crash or freeze",
        "Operation completes or shows clear dialog",
        "After closing Bundle, check Task Manager for zombie EXCEL.EXE processes"
    )

# Auto-check if Excel is running
$excelProcess = Get-Process -Name "EXCEL" -ErrorAction SilentlyContinue
if ($excelProcess) {
    Write-Host "??  AUTO-CHECK: Excel IS currently running (PID: $($excelProcess.Id))" -ForegroundColor Cyan
    Write-Host "   Test will use existing instance" -ForegroundColor White
} else {
  Write-Host "? AUTO-CHECK: Excel is NOT running" -ForegroundColor Green
    Write-Host "   Test will create new instance" -ForegroundColor White
}
Write-Host ""

Write-Host "Do you want to test Excel integration? (Y/N/S)" -ForegroundColor Yellow
$testExcel = Read-Host
if ($testExcel -eq 'S' -or $testExcel -eq 's') {
    $result = "SKIP"
} else {
    $result = Get-TestResult
}
$testResults += "Test 2.5: $result"
if ($result -eq "PASS") { $passCount++ } 
elseif ($result -like "FAIL*") { $failCount++ }
else { $skipCount++ }

#???????????????????????????????????????????????????????????
# TEST SUMMARY
#???????????????????????????????????????????????????????????

Clear-Host
Write-Host ""
Write-Host "?????????????????????????????????????????????????????????????" -ForegroundColor Cyan
Write-Host "?     TEST SUMMARY    ?" -ForegroundColor Cyan
Write-Host "?????????????????????????????????????????????????????????????" -ForegroundColor Cyan
Write-Host ""

$totalTests = $testResults.Count
Write-Host "Total Tests Run: $totalTests" -ForegroundColor White
Write-Host "  ? Passed: $passCount" -ForegroundColor Green
Write-Host "  ? Failed: $failCount" -ForegroundColor Red
Write-Host "  ??  Skipped: $skipCount" -ForegroundColor Yellow
Write-Host ""

Write-Host "???????????????????????????????????????????????????????????" -ForegroundColor Gray
Write-Host "DETAILED RESULTS:" -ForegroundColor White
Write-Host "???????????????????????????????????????????????????????????" -ForegroundColor Gray
foreach ($result in $testResults) {
    if ($result -like "*PASS*") {
        Write-Host "  ? $result" -ForegroundColor Green
    } elseif ($result -like "*FAIL*") {
        Write-Host "  ? $result" -ForegroundColor Red
    } else {
        Write-Host "  ??  $result" -ForegroundColor Yellow
    }
}
Write-Host ""

# Overall assessment
Write-Host "???????????????????????????????????????????????????????????" -ForegroundColor Cyan
if ($failCount -eq 0 -and $passCount -ge ($totalTests * 0.8)) {
    Write-Host "?? EXCELLENT! Core functionality verified!" -ForegroundColor Green
    Write-Host ""
    Write-Host "The refactoring appears to be working correctly." -ForegroundColor White
    Write-Host "You can proceed with confidence!" -ForegroundColor Green
} elseif ($failCount -le 1 -and $passCount -ge ($totalTests * 0.6)) {
    Write-Host "? GOOD! Most tests passed" -ForegroundColor Yellow
    Write-Host ""
    Write-Host "Minor issues detected. Review failures above." -ForegroundColor White
} else {
    Write-Host "??  NEEDS ATTENTION" -ForegroundColor Red
    Write-Host ""
    Write-Host "Several tests failed. Please review the issues." -ForegroundColor White
}
Write-Host ""

# Save results
$reportPath = "TEST_RESULTS_$(Get-Date -Format 'yyyyMMdd_HHmmss').txt"
$testResults | Out-File $reportPath
Write-Host "Test results saved to: $reportPath" -ForegroundColor Cyan
Write-Host ""

# Next steps
Write-Host "???????????????????????????????????????????????????????????" -ForegroundColor Cyan
Write-Host "NEXT STEPS:" -ForegroundColor Yellow
Write-Host ""
if ($failCount -eq 0) {
    Write-Host "? All tests passed! You can:" -ForegroundColor Green
    Write-Host "   1. Continue using Bundle with confidence" -ForegroundColor White
    Write-Host "   2. Run full test suite (see TESTING_GUIDE.md)" -ForegroundColor White
    Write-Host "   3. Start migrating other projects" -ForegroundColor White
} else {
    Write-Host "Fix failed tests:" -ForegroundColor Yellow
  Write-Host "   1. Review error messages above" -ForegroundColor White
    Write-Host "   2. Check log files for details" -ForegroundColor White
    Write-Host " 3. Consult TESTING_GUIDE.md for troubleshooting" -ForegroundColor White
    Write-Host "   4. Re-run this test after fixes" -ForegroundColor White
}
Write-Host ""

Write-Host "???????????????????????????????????????????????????????????" -ForegroundColor Cyan
Write-Host "Testing complete! Thank you!" -ForegroundColor White
Write-Host ""
