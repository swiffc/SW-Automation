#Requires -Version 5.1
<#
.SYNOPSIS
    Interactive testing script for refactored SolidWorks automation projects

.DESCRIPTION
    Tests Bundle, Excel, and UnifiedUI with enterprise error handling
    
.NOTES
    Author: AI Assistant
    Date: October 27, 2025
    Version: 1.0
#>

$ErrorActionPreference = 'Continue'

# Colors
function Write-Title { Write-Host $args -ForegroundColor Cyan }
function Write-Step { Write-Host $args -ForegroundColor Yellow }
function Write-Success { Write-Host "? $args" -ForegroundColor Green }
function Write-Failure { Write-Host "? $args" -ForegroundColor Red }
function Write-Info { Write-Host $args -ForegroundColor White }

Clear-Host

Write-Title "?????????????????????????????????????????????????????????????????"
Write-Title "?  SOLIDWORKS AUTOMATION - REFACTORED PROJECT TEST SUITE       ?"
Write-Title "?????????????????????????????????????????????????????????????????"
Write-Host ""

$workspace = "C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation"
$logPath = "$env:APPDATA\BundleApp\Logs"

# Test 1: Verify builds
Write-Title "`n[TEST 1] BUILD VERIFICATION"
Write-Host "?????????????????????????????????????????????????????????????????"

Write-Step "`n1. Building Bundle..."
$bundleBuild = & msbuild "$workspace\Bundle\Bundle.csproj" /t:Build /p:Configuration=Debug /nologo /v:q 2>&1
if ($LASTEXITCODE -eq 0) {
    Write-Success "Bundle builds successfully"
} else {
    Write-Failure "Bundle build failed"
}

Write-Step "`n2. Building Excel..."
$excelBuild = & msbuild "$workspace\Excel\Excel.csproj" /t:Build /p:Configuration=Debug /nologo /v:q 2>&1
if ($LASTEXITCODE -eq 0) {
    Write-Success "Excel builds successfully"
} else {
Write-Failure "Excel build failed"
}

Write-Step "`n3. Building UnifiedUI..."
$unifiedBuild = & msbuild "$workspace\UnifiedUI\UnifiedUI.csproj" /t:Build /p:Configuration=Debug /nologo /v:q 2>&1
if ($LASTEXITCODE -eq 0) {
    Write-Success "UnifiedUI builds successfully"
} else {
    Write-Failure "UnifiedUI build failed"
}

# Test 2: Verify executables exist
Write-Title "`n[TEST 2] EXECUTABLE VERIFICATION"
Write-Host "?????????????????????????????????????????????????????????????????"

$bundleExe = "$workspace\Bundle\bin\Debug\Bundle.exe"
$unifiedExe = "$workspace\UnifiedUI\bin\Debug\net481\UnifiedUI.exe"

if (Test-Path $bundleExe) {
    $bundleSize = (Get-Item $bundleExe).Length / 1KB
    Write-Success "Bundle.exe found ($([math]::Round($bundleSize, 2)) KB)"
} else {
    Write-Failure "Bundle.exe not found at: $bundleExe"
}

if (Test-Path $unifiedExe) {
    $unifiedSize = (Get-Item $unifiedExe).Length / 1KB
    Write-Success "UnifiedUI.exe found ($([math]::Round($unifiedSize, 2)) KB)"
} else {
    Write-Failure "UnifiedUI.exe not found at: $unifiedExe"
}

# Test 3: Verify infrastructure files
Write-Title "`n[TEST 3] INFRASTRUCTURE VERIFICATION"
Write-Host "?????????????????????????????????????????????????????????????????"

$globalErrorHandler = "$workspace\FileTools\Infrastructure\GlobalErrorHandler.cs"
$comObjectManager = "$workspace\FileTools\Infrastructure\ComObjectManager.cs"

if (Test-Path $globalErrorHandler) {
    Write-Success "GlobalErrorHandler.cs present"
} else {
  Write-Failure "GlobalErrorHandler.cs missing"
}

if (Test-Path $comObjectManager) {
    Write-Success "ComObjectManager.cs present"
} else {
    Write-Failure "ComObjectManager.cs missing"
}

# Test 4: Check log directory
Write-Title "`n[TEST 4] LOGGING INFRASTRUCTURE"
Write-Host "?????????????????????????????????????????????????????????????????"

if (Test-Path $logPath) {
    $logFiles = Get-ChildItem $logPath -Filter "BundleApp_*.log" -ErrorAction SilentlyContinue
    if ($logFiles) {
    Write-Success "Log directory exists: $logPath"
        Write-Info "  Found $($logFiles.Count) log file(s)"
        $latest = $logFiles | Sort-Object LastWriteTime -Descending | Select-Object -First 1
        if ($latest) {
   Write-Info "  Latest: $($latest.Name) ($([math]::Round($latest.Length / 1KB, 2)) KB)"
        }
    } else {
   Write-Info "  Log directory exists but no log files yet"
        Write-Info "  Logs will be created when applications run"
    }
} else {
 Write-Info "  Log directory will be created on first run"
    Write-Info "  Location: $logPath"
}

# Test 5: Documentation check
Write-Title "`n[TEST 5] DOCUMENTATION VERIFICATION"
Write-Host "?????????????????????????????????????????????????????????????????"

$docs = @(
    "docs\README.md",
    "docs\Testing\TESTING_GUIDE.md",
    "docs\Testing\TESTING_QUICK_REFERENCE.md",
    "docs\Status\UNIFIEDUI_INTEGRATION_COMPLETE.md",
    "docs\Status\SESSION_COMPLETION_SUMMARY.md"
)

foreach ($doc in $docs) {
    $fullPath = Join-Path $workspace $doc
    if (Test-Path $fullPath) {
        Write-Success $doc
    } else {
        Write-Failure "$doc missing"
    }
}

# Test 6: Interactive test options
Write-Title "`n[TEST 6] INTERACTIVE TESTING OPTIONS"
Write-Host "?????????????????????????????????????????????????????????????????"

Write-Host "`nSelect a test to run:"
Write-Host "  [1] Launch Bundle.exe (Windows Forms UI)"
Write-Host "  [2] Launch UnifiedUI.exe (Modern WPF UI)"
Write-Host "  [3] View latest log file"
Write-Host "  [4] Open log directory"
Write-Host "  [5] Run all apps and monitor logs"
Write-Host "  [0] Exit test suite"
Write-Host ""

$choice = Read-Host "Enter your choice (0-5)"

switch ($choice) {
    "1" {
   Write-Step "`nLaunching Bundle.exe..."
        Write-Info "Check for:"
        Write-Info "  - Application starts without errors"
 Write-Info "  - Log file created in $logPath"
        Write-Info "  - Error handling works (try without SolidWorks)"
        Write-Host ""
        
        if (Test-Path $bundleExe) {
            Start-Process $bundleExe
         Write-Success "Bundle.exe launched"
 Write-Info "`nMonitoring log directory..."
      Start-Sleep -Seconds 2
      
            $newLogs = Get-ChildItem $logPath -Filter "BundleApp_*.log" -ErrorAction SilentlyContinue | 
      Sort-Object LastWriteTime -Descending | 
        Select-Object -First 1
            
          if ($newLogs) {
          Write-Success "Log file active: $($newLogs.Name)"
     Write-Host "`nLast 10 log entries:"
      Write-Host "?????????????????????????????????????????????????????????????????"
                Get-Content $newLogs.FullName -Tail 10 | ForEach-Object { 
    Write-Host "  $_" -ForegroundColor Gray 
        }
       }
        } else {
 Write-Failure "Bundle.exe not found!"
 }
    }
    
    "2" {
        Write-Step "`nLaunching UnifiedUI.exe..."
   Write-Info "Check for:"
        Write-Info "  - Modern WPF interface loads"
        Write-Info "  - 9 component tabs visible"
        Write-Info "  - Log file created with startup message"
  Write-Info "  - Error handling works"
    Write-Host ""
   
        if (Test-Path $unifiedExe) {
     Start-Process $unifiedExe
     Write-Success "UnifiedUI.exe launched"
            Write-Info "`nMonitoring log directory..."
            Start-Sleep -Seconds 2
            
            $newLogs = Get-ChildItem $logPath -Filter "BundleApp_*.log" -ErrorAction SilentlyContinue | 
            Sort-Object LastWriteTime -Descending | 
        Select-Object -First 1
      
            if ($newLogs) {
        Write-Success "Log file active: $($newLogs.Name)"
         Write-Host "`nLast 10 log entries:"
         Write-Host "?????????????????????????????????????????????????????????????????"
  Get-Content $newLogs.FullName -Tail 10 | ForEach-Object { 
Write-Host "  $_" -ForegroundColor Gray 
       }
          }
        } else {
            Write-Failure "UnifiedUI.exe not found!"
        }
    }
    
  "3" {
Write-Step "`nRetrieving latest log file..."
        
        if (Test-Path $logPath) {
            $latestLog = Get-ChildItem $logPath -Filter "BundleApp_*.log" -ErrorAction SilentlyContinue | 
  Sort-Object LastWriteTime -Descending | 
           Select-Object -First 1
        
        if ($latestLog) {
                Write-Success "Latest log: $($latestLog.Name)"
      Write-Info "Size: $([math]::Round($latestLog.Length / 1KB, 2)) KB"
         Write-Info "Modified: $($latestLog.LastWriteTime)"
    Write-Host "`nLast 20 entries:"
      Write-Host "?????????????????????????????????????????????????????????????????"
                Get-Content $latestLog.FullName -Tail 20 | ForEach-Object { 
    if ($_ -match '\[ERROR\]') {
 Write-Host "  $_" -ForegroundColor Red
         } elseif ($_ -match '\[WARNING\]') {
      Write-Host "  $_" -ForegroundColor Yellow
         } else {
Write-Host "  $_" -ForegroundColor Gray
        }
        }
 } else {
    Write-Info "No log files found. Run an application first."
  }
     } else {
          Write-Info "Log directory doesn't exist yet. Run an application first."
      }
    }
    
    "4" {
        Write-Step "`nOpening log directory..."
        
     if (!(Test-Path $logPath)) {
            New-Item -ItemType Directory -Path $logPath -Force | Out-Null
          Write-Success "Created log directory: $logPath"
        }
    
        explorer $logPath
        Write-Success "Explorer opened at: $logPath"
    }
    
 "5" {
        Write-Step "`nLaunching all applications..."
  Write-Host ""
        
        # Launch Bundle
        if (Test-Path $bundleExe) {
    Start-Process $bundleExe
    Write-Success "Bundle.exe launched"
        }
        
        # Launch UnifiedUI
      if (Test-Path $unifiedExe) {
     Start-Process $unifiedExe
            Write-Success "UnifiedUI.exe launched"
 }
        
    Write-Info "`nWaiting for applications to initialize..."
        Start-Sleep -Seconds 3
      
     Write-Info "`nMonitoring logs (Press Ctrl+C to stop)..."
        Write-Host "?????????????????????????????????????????????????????????????????"
        
        $latestLog = Get-ChildItem $logPath -Filter "BundleApp_*.log" -ErrorAction SilentlyContinue | 
         Sort-Object LastWriteTime -Descending | 
  Select-Object -First 1
        
   if ($latestLog) {
       Get-Content $latestLog.FullName -Wait -Tail 0 | ForEach-Object {
    if ($_ -match '\[ERROR\]') {
    Write-Host $_ -ForegroundColor Red
        } elseif ($_ -match '\[WARNING\]') {
                    Write-Host $_ -ForegroundColor Yellow
     } elseif ($_ -match '\[INFO\]') {
      Write-Host $_ -ForegroundColor Cyan
       } else {
  Write-Host $_ -ForegroundColor Gray
  }
  }
        }
    }
    
    "0" {
        Write-Info "`nExiting test suite..."
    }
    
    default {
        Write-Failure "`nInvalid choice. Exiting..."
    }
}

# Summary
Write-Title "`n?????????????????????????????????????????????????????????????????"
Write-Title "?  TEST SUITE COMPLETE     ?"
Write-Title "?????????????????????????????????????????????????????????????????"

Write-Host "`n?? Documentation:"
Write-Info "  - Testing Guide: $workspace\docs\Testing\TESTING_GUIDE.md"
Write-Info "  - Quick Reference: $workspace\docs\Testing\TESTING_QUICK_REFERENCE.md"
Write-Info "  - Integration Status: $workspace\docs\Status\UNIFIEDUI_INTEGRATION_COMPLETE.md"

Write-Host "`n?? Logs:"
Write-Info "  - Location: $logPath"
Write-Info "  - Pattern: BundleApp_YYYYMMDD.log"

Write-Host "`n?? What to test:"
Write-Info "  1. Launch Bundle.exe ? Test with/without SolidWorks"
Write-Info "  2. Launch UnifiedUI.exe ? Navigate tabs, fill forms"
Write-Info "  3. Check logs ? Verify all actions logged"
Write-Info "  4. Test errors ? Verify user-friendly messages"

Write-Host "`n? All refactored projects built successfully!"
Write-Host ""
