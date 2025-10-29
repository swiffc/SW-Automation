# TEST DROPDOWNS - Diagnostic Script
Write-Host "`n========================================" -ForegroundColor Cyan
Write-Host "  DROPDOWN DIAGNOSTIC TEST" -ForegroundColor Yellow
Write-Host "========================================`n" -ForegroundColor Cyan

# Step 1: Check templates exist
Write-Host "[1/5] Checking templates folder..." -ForegroundColor White
if (Test-Path "templates") {
    Write-Host "  ? templates\ exists" -ForegroundColor Green
    
    $headerCount = (Get-ChildItem "templates\header_section_tool" -Filter "*.SLDASM" -Recurse).Count
    $xchCount = (Get-ChildItem "templates\xch_structure_tool" -Filter "*.SLDASM" -Recurse).Count
    $zCount = (Get-ChildItem "templates\z_structure_tool" -Filter "*.SLDASM" -Recurse).Count
    $hudsonCount = (Get-ChildItem "templates\hudson_certified" -Filter "*.SLDASM" -Recurse).Count
    
    Write-Host "  ? Header: $headerCount templates" -ForegroundColor Green
    Write-Host "  ? XCH: $xchCount templates" -ForegroundColor Green
    Write-Host "  ? Z: $zCount templates" -ForegroundColor Green
    Write-Host "  ? Hudson: $hudsonCount templates`n" -ForegroundColor Green
} else {
    Write-Host "  ? templates\ folder NOT FOUND!`n" -ForegroundColor Red
    exit 1
}

# Step 2: Check deploy folder
Write-Host "[2/5] Checking deploy folder..." -ForegroundColor White
if (Test-Path "deploy\UnifiedUI\UnifiedUI.exe") {
    Write-Host "  ? UnifiedUI.exe exists" -ForegroundColor Green
    $exeInfo = Get-Item "deploy\UnifiedUI\UnifiedUI.exe"
    Write-Host "  ? Size: $([math]::Round($exeInfo.Length/1MB, 2)) MB" -ForegroundColor Green
    Write-Host "  ? Modified: $($exeInfo.LastWriteTime)`n" -ForegroundColor Green
} else {
    Write-Host "  ? UnifiedUI.exe NOT FOUND!" -ForegroundColor Red
    Write-Host "  Run: .\BUILD-AND-DEPLOY.ps1`n" -ForegroundColor Yellow
    exit 1
}

# Step 3: Check for DLLs
Write-Host "[3/5] Checking required DLLs..." -ForegroundColor White
$requiredDLLs = @(
    "FileTools.dll",
    "Excel.dll",
    "Bundle.dll",
    "Header.dll"
)

$allFound = $true
foreach ($dll in $requiredDLLs) {
    $path = "deploy\UnifiedUI\$dll"
    if (Test-Path $path) {
        Write-Host "  ? $dll" -ForegroundColor Green
    } else {
        Write-Host "  ? $dll MISSING!" -ForegroundColor Red
        $allFound = $false
    }
}

if (!$allFound) {
    Write-Host "`n  ?? Missing DLLs - rebuild required!`n" -ForegroundColor Yellow
    exit 1
}

Write-Host ""

# Step 4: Clean old logs
Write-Host "[4/5] Cleaning old log files..." -ForegroundColor White
$oldLogs = Get-ChildItem "$env:TEMP" -Filter "*UnifiedUI*.log" -ErrorAction SilentlyContinue
if ($oldLogs) {
    $oldLogs | Remove-Item -Force
    Write-Host "  ? Removed $($oldLogs.Count) old log files`n" -ForegroundColor Green
} else {
    Write-Host "  ? No old logs to clean`n" -ForegroundColor Green
}

# Step 5: Launch with monitoring
Write-Host "[5/5] Launching UnifiedUI..." -ForegroundColor White
Write-Host "  Starting application..." -ForegroundColor Gray

$process = Start-Process -FilePath "deploy\UnifiedUI\UnifiedUI.exe" -PassThru

Write-Host "  ? Process started (PID: $($process.Id))`n" -ForegroundColor Green

# Wait a bit for initialization
Write-Host "Waiting 3 seconds for initialization..." -ForegroundColor Yellow
Start-Sleep -Seconds 3

# Check for log file
Write-Host "`nChecking for log file..." -ForegroundColor White
$newLogs = Get-ChildItem "$env:TEMP" -Filter "*UnifiedUI*.log" -ErrorAction SilentlyContinue | 
    Sort-Object LastWriteTime -Descending

if ($newLogs) {
    $logFile = $newLogs[0]
    Write-Host "  ? Log file created: $($logFile.Name)" -ForegroundColor Green
    Write-Host "`n========================================" -ForegroundColor Cyan
    Write-Host "  LOG FILE CONTENTS (Last 50 lines)" -ForegroundColor Yellow
    Write-Host "========================================`n" -ForegroundColor Cyan
    Get-Content $logFile.FullName -Tail 50
    
    Write-Host "`n========================================" -ForegroundColor Cyan
    Write-Host "  KEY INDICATORS TO CHECK" -ForegroundColor Yellow
    Write-Host "========================================`n" -ForegroundColor Cyan
    
    $logContent = Get-Content $logFile.FullName -Raw
    
    if ($logContent -match "AvailableTools") {
        Write-Host "  ? AvailableTools found in log" -ForegroundColor Green
    } else {
        Write-Host "  ? AvailableTools NOT in log" -ForegroundColor Red
    }
    
    if ($logContent -match "Switching to tool") {
        Write-Host "  ? Tool switching detected" -ForegroundColor Green
    } else {
        Write-Host "  ? No tool switching found" -ForegroundColor Red
    }
    
    if ($logContent -match "Loading templates") {
        Write-Host "  ? Template loading attempted" -ForegroundColor Green
    } else {
        Write-Host "  ? No template loading found" -ForegroundColor Red
    }
    
    if ($logContent -match "Loaded \d+ templates") {
        Write-Host "  ? Templates loaded successfully" -ForegroundColor Green
    } else {
        Write-Host "  ? Templates did NOT load" -ForegroundColor Red
    }
    
    $errors = $logContent | Select-String -Pattern "ERROR|Exception" -AllMatches
    if ($errors) {
        Write-Host "  ?? Found $($errors.Matches.Count) errors in log!" -ForegroundColor Red
    } else {
        Write-Host "  ? No errors in log" -ForegroundColor Green
    }
    
} else {
    Write-Host "  ?? No log file created yet" -ForegroundColor Yellow
    Write-Host "    Check if GlobalErrorHandler is initialized`n" -ForegroundColor Gray
}

Write-Host "`n========================================" -ForegroundColor Cyan
Write-Host "  TEST YOUR DROPDOWNS NOW!" -ForegroundColor Green
Write-Host "========================================`n" -ForegroundColor Cyan

Write-Host "1. Click the 'Project/Tool' dropdown" -ForegroundColor White
Write-Host "   Expected: Shows 4 tools`n" -ForegroundColor Gray

Write-Host "2. Select different tools" -ForegroundColor White
Write-Host "   Expected: Tabs change, templates load`n" -ForegroundColor Gray

Write-Host "3. Check template dropdown (in Bundle tab)" -ForegroundColor White
Write-Host "   Expected: Shows 16 templates for Header tool`n" -ForegroundColor Gray

Write-Host "Press CTRL+C to stop monitoring`n" -ForegroundColor Yellow

Write-Host "Log file location:" -ForegroundColor Cyan
if ($newLogs) {
    Write-Host "  $($newLogs[0].FullName)`n" -ForegroundColor White
}

Write-Host "========================================`n" -ForegroundColor Cyan


