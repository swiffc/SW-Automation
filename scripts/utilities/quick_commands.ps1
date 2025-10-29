# Quick Commands for AI Agents
# Source: . .\.cursor\quick_commands.ps1

# Build UnifiedUI only (fast)
function Build-UnifiedUI {
    cd C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation
    & "C:\Program Files\Microsoft Visual Studio\2022\Professional\MSBuild\Current\Bin\MSBuild.exe" `
      UnifiedUI\UnifiedUI.csproj /p:Configuration=Debug /v:q /nologo 2>&1 | Select-String "error|Build succeeded"
}

# Build entire solution
function Build-All {
    cd C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation
    & "C:\Program Files\Microsoft Visual Studio\2022\Professional\MSBuild\Current\Bin\MSBuild.exe" `
      "Solidworks Automation.sln" /p:Configuration=Debug /v:minimal /nologo
}

# Kill all running instances
function Stop-All {
    Stop-Process -Name "UnifiedUI","Bundle","Header","Hood","Structure","Walkway" -Force -ErrorAction SilentlyContinue
    Write-Host "? All processes stopped" -ForegroundColor Green
}

# Quick test UnifiedUI
function Test-UnifiedUI {
    Stop-All
    Start-Sleep -Seconds 1
    & "C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation\UnifiedUI\bin\Debug\net481\UnifiedUI.exe"
}

# Quick test Bundle
function Test-Bundle {
    Stop-All
    Start-Sleep -Seconds 1
    & "C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation\Bundle\bin\Debug\net481\Bundle.exe"
}

# Find all TODOs
function Find-TODOs {
    cd C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation
    Select-String -Path *.cs -Pattern "TODO|FIXME|HACK" -Recurse -CaseSensitive | 
        Select-Object Filename, LineNumber, Line | 
        Format-Table -AutoSize
}

# Find all AXC_VAULT references
function Find-VaultRefs {
    cd C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation
    Select-String -Path *.cs -Pattern "AXC_VAULT" -Recurse | 
        Select-Object Filename, LineNumber | 
        Format-Table -AutoSize
}

# Check for running SolidWorks processes
function Check-SolidWorks {
    $sw = Get-Process | Where-Object { $_.ProcessName -eq "SLDWORKS" }
    if ($sw) {
        Write-Host "? SolidWorks is running (PID: $($sw.Id))" -ForegroundColor Green
    } else {
        Write-Host "? SolidWorks is NOT running" -ForegroundColor Red
    }
}

# Check for running Excel processes
function Check-Excel {
    $excel = Get-Process | Where-Object { $_.ProcessName -eq "EXCEL" }
    if ($excel) {
        Write-Host "?? Excel is running (PID: $($excel.Id)) - may need to close for COM cleanup" -ForegroundColor Yellow
    } else {
        Write-Host "? Excel is NOT running" -ForegroundColor Green
    }
}

# Clean all bin/obj folders (when build is broken)
function Clean-All {
    cd C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation
    Write-Host "Cleaning all bin/obj folders..." -ForegroundColor Yellow
    Get-ChildItem -Recurse -Include bin,obj -Directory | Remove-Item -Recurse -Force
    Write-Host "? Clean complete" -ForegroundColor Green
}

# Show recent log file
function Show-Log {
    param([string]$Component = "UnifiedUI")
    $logPath = "$env:APPDATA\$Component`App\Logs"
    if (Test-Path $logPath) {
        $latestLog = Get-ChildItem $logPath -Filter "*.log" | Sort-Object LastWriteTime -Descending | Select-Object -First 1
        if ($latestLog) {
            Write-Host "Latest log: $($latestLog.FullName)" -ForegroundColor Cyan
            Get-Content $latestLog.FullName -Tail 50
        }
    } else {
        Write-Host "? No log directory found at $logPath" -ForegroundColor Red
    }
}

# Quick status check
function Check-Status {
    Write-Host "`n=== PROJECT STATUS ===" -ForegroundColor Cyan
    Write-Host "`nProcesses:" -ForegroundColor Yellow
    Get-Process | Where-Object { $_.ProcessName -in @("UnifiedUI","Bundle","Header","SLDWORKS","EXCEL") } | 
        Format-Table ProcessName, Id, StartTime -AutoSize
    
    Write-Host "`nLast Build:" -ForegroundColor Yellow
    $exePath = "C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation\UnifiedUI\bin\Debug\net481\UnifiedUI.exe"
    if (Test-Path $exePath) {
        $buildTime = (Get-Item $exePath).LastWriteTime
        Write-Host "  UnifiedUI.exe: $buildTime" -ForegroundColor White
    }
    
    Write-Host ""
}

# Export all functions
Export-ModuleMember -Function *

Write-Host "? Quick commands loaded!" -ForegroundColor Green
Write-Host "Available commands:" -ForegroundColor Cyan
Write-Host "  Build-UnifiedUI   - Fast build of UnifiedUI only" -ForegroundColor White
Write-Host "  Build-All         - Build entire solution" -ForegroundColor White
Write-Host "  Stop-All          - Kill all running processes" -ForegroundColor White
Write-Host "  Test-UnifiedUI    - Launch UnifiedUI" -ForegroundColor White
Write-Host "  Test-Bundle       - Launch Bundle" -ForegroundColor White
Write-Host "  Find-TODOs        - Search for TODO/FIXME" -ForegroundColor White
Write-Host "  Find-VaultRefs    - Find AXC_VAULT references" -ForegroundColor White
Write-Host "  Check-SolidWorks  - Check if SolidWorks is running" -ForegroundColor White
Write-Host "  Check-Excel       - Check if Excel is running" -ForegroundColor White
Write-Host "  Clean-All         - Delete all bin/obj folders" -ForegroundColor White
Write-Host "  Show-Log          - Show recent log file" -ForegroundColor White
Write-Host "  Check-Status      - Show project status" -ForegroundColor White

