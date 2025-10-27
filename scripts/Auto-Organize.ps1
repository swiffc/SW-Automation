# Auto-Organize File Watcher
# Purpose: Automatically move files to correct locations when they're created
# Usage: Run this in background to continuously monitor and organize files

param(
    [switch]$Install = $false,
    [switch]$Uninstall = $false,
    [int]$CheckIntervalSeconds = 30
)

$ErrorActionPreference = "Stop"
$RootPath = Split-Path -Parent (Split-Path -Parent $PSScriptRoot)

# File organization rules
$OrganizationRules = @{
    # Python files go to utilities/python/
    "*.py" = @{
        Destination = "utilities/python"
        Exclude = @("setup.py", "test_*.py") # Keep these in root/tests
    }
    
    # PowerShell scripts by prefix
    "SETUP_*.ps1" = @{
        Destination = "scripts/setup"
    }
    "COPY_*.ps1" = @{
        Destination = "scripts/utilities"
    }
    "Test-*.ps1" = @{
        Destination = "scripts/testing"
    }
    "Build-*.ps1" = @{
        Destination = "scripts/build"
    }
    
    # Excel analysis files
    "*EXCEL*.json" = @{
        Destination = "analysis/excel"
    }
    "*CELL_MAPPING*.json" = @{
        Destination = "analysis/excel"
    }
    "*CELL_MAPPING*.md" = @{
        Destination = "analysis/excel"
    }
    
    # Sample/example files
    "*.xlsm" = @{
        Destination = "examples"
        Exclude = @() # Move all xlsm to examples
    }
    
    # Temporary files (delete instead of move)
    "*_temp.txt" = @{
        Action = "Delete"
    }
    "scan_complete.txt" = @{
        Action = "Delete"
    }
    "BUILD_STATUS.txt" = @{
        Action = "Delete"
    }
}

function Write-ColorOutput {
    param(
        [string]$Message,
        [string]$Color = "White"
    )
    $timestamp = Get-Date -Format "HH:mm:ss"
    Write-Host "[$timestamp] " -NoNewline -ForegroundColor Gray
    Write-Host $Message -ForegroundColor $Color
}

function Test-ShouldExclude {
    param(
        [string]$FileName,
        [array]$ExcludeList
    )
    
    foreach ($pattern in $ExcludeList) {
        if ($FileName -like $pattern) {
            return $true
        }
    }
    return $false
}

function Move-FileToCorrectLocation {
    param(
        [string]$FilePath
    )
    
    $fileName = Split-Path -Leaf $FilePath
    $moved = $false
    
    foreach ($pattern in $OrganizationRules.Keys) {
        if ($fileName -like $pattern) {
            $rule = $OrganizationRules[$pattern]
            
            # Check exclusions
            if ($rule.Exclude -and (Test-ShouldExclude -FileName $fileName -ExcludeList $rule.Exclude)) {
                Write-ColorOutput "Skipped: $fileName (excluded)" -Color DarkGray
                continue
            }
            
            # Handle delete action
            if ($rule.Action -eq "Delete") {
                try {
                    Remove-Item -Path $FilePath -Force
                    Write-ColorOutput "Deleted: $fileName" -Color Red
                    return $true
                }
                catch {
                    Write-ColorOutput "Error deleting $fileName`: $_" -Color Red
                    return $false
                }
            }
            
            # Handle move action
            if ($rule.Destination) {
                $destFolder = Join-Path $RootPath $rule.Destination
                
                # Create destination folder if it doesn't exist
                if (-not (Test-Path $destFolder)) {
                    New-Item -ItemType Directory -Path $destFolder -Force | Out-Null
                    Write-ColorOutput "Created folder: $($rule.Destination)" -Color Yellow
                }
                
                $destPath = Join-Path $destFolder $fileName
                
                # Move file
                try {
                    Move-Item -Path $FilePath -Destination $destPath -Force
                    Write-ColorOutput "Moved: $fileName -> $($rule.Destination)/" -Color Green
                    return $true
                }
                catch {
                    Write-ColorOutput "Error moving $fileName`: $_" -Color Red
                    return $false
                }
            }
        }
    }
    
    return $moved
}

function Start-FileWatcher {
    Write-ColorOutput "Starting Auto-Organize File Watcher..." -Color Cyan
    Write-ColorOutput "Monitoring: $RootPath" -Color Cyan
    Write-ColorOutput "Check interval: $CheckIntervalSeconds seconds" -Color Cyan
    Write-ColorOutput "Press Ctrl+C to stop`n" -Color Yellow
    
    $iteration = 0
    
    while ($true) {
        $iteration++
        
        # Get all files in root (not in subdirectories)
        $files = Get-ChildItem -Path $RootPath -File -ErrorAction SilentlyContinue | Where-Object {
            # Exclude files that should stay in root
            $_.Name -notin @("README.md", ".gitignore", "requirements.txt", "config.json", "LICENSE", "CONTRIBUTING.md", ".git")
        }
        
        if ($files.Count -gt 0) {
            Write-ColorOutput "Found $($files.Count) file(s) to organize..." -Color Yellow
            
            foreach ($file in $files) {
                Move-FileToCorrectLocation -FilePath $file.FullName
            }
            
            Write-ColorOutput "Scan complete. Organized $($files.Count) file(s)`n" -Color Green
        }
        else {
            # Periodic status update (every 10 iterations)
            if ($iteration % 10 -eq 0) {
                Write-ColorOutput "Monitoring... (no files to organize)" -Color DarkGray
            }
        }
        
        Start-Sleep -Seconds $CheckIntervalSeconds
    }
}

function Install-AutoOrganizer {
    Write-Host "`n=== Installing Auto-Organizer ===" -ForegroundColor Cyan
    
    # Create a scheduled task to run at startup
    $scriptPath = $PSCommandPath
    $taskName = "SolidWorks-AutoOrganize"
    
    # Check if task already exists
    $existingTask = Get-ScheduledTask -TaskName $taskName -ErrorAction SilentlyContinue
    if ($existingTask) {
        Write-Host "Task already exists. Updating..." -ForegroundColor Yellow
        Unregister-ScheduledTask -TaskName $taskName -Confirm:$false
    }
    
    # Create scheduled task
    $action = New-ScheduledTaskAction -Execute "powershell.exe" -Argument "-WindowStyle Hidden -ExecutionPolicy Bypass -File `"$scriptPath`""
    $trigger = New-ScheduledTaskTrigger -AtStartup
    $settings = New-ScheduledTaskSettingsSet -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries -StartWhenAvailable
    
    try {
        Register-ScheduledTask -TaskName $taskName -Action $action -Trigger $trigger -Settings $settings -Description "Auto-organize SolidWorks project files" -RunLevel Limited
        Write-Host "`nInstalled successfully!" -ForegroundColor Green
        Write-Host "The auto-organizer will start automatically when Windows starts." -ForegroundColor Green
        Write-Host "`nTo start now: Start-ScheduledTask -TaskName '$taskName'" -ForegroundColor Yellow
    }
    catch {
        Write-Host "Error installing: $_" -ForegroundColor Red
    }
}

function Uninstall-AutoOrganizer {
    Write-Host "`n=== Uninstalling Auto-Organizer ===" -ForegroundColor Cyan
    
    $taskName = "SolidWorks-AutoOrganize"
    $existingTask = Get-ScheduledTask -TaskName $taskName -ErrorAction SilentlyContinue
    
    if ($existingTask) {
        Unregister-ScheduledTask -TaskName $taskName -Confirm:$false
        Write-Host "Uninstalled successfully!" -ForegroundColor Green
    }
    else {
        Write-Host "Task not found. Nothing to uninstall." -ForegroundColor Yellow
    }
}

# Main execution
if ($Install) {
    Install-AutoOrganizer
}
elseif ($Uninstall) {
    Uninstall-AutoOrganizer
}
else {
    # Run the file watcher
    Start-FileWatcher
}

