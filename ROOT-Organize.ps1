# ROOT-Organize.ps1
# Comprehensive Root Folder Cleanup Script
# Enforces .cursorrules file organization rules
# Run from: Solidworks_Automation root folder

Write-Host "`n========================================" -ForegroundColor Cyan
Write-Host "   ROOT FOLDER ORGANIZATION" -ForegroundColor Green
Write-Host "========================================`n" -ForegroundColor Cyan

# Define allowed files in root
$allowedInRoot = @(
    "README.md",
    ".gitignore",
    ".cursorrules",
    "requirements.txt",
    "config.json",
    "ROOT-Organize.ps1",
    "LICENSE",
    "AGENTS.md"  # Special exception for agent guide
)

Write-Host "Creating folder structure..." -ForegroundColor Yellow

# Create comprehensive folder structure
$folders = @(
    "scripts/setup",
    "scripts/utilities",
    "scripts/testing",
    "scripts/build",
    "utilities/python",
    "analysis/excel",
    "examples",
    "docs/Getting-Started",
    "docs/Integration",
    "docs/Architecture",
    "docs/Testing",
    "docs/Reference",
    "docs/Status",
    "docs/Migration"
)

foreach ($folder in $folders) {
    if (-not (Test-Path $folder)) {
        New-Item -ItemType Directory -Path $folder -Force | Out-Null
        Write-Host "  ? Created: $folder" -ForegroundColor Green
    }
}

Write-Host "`nMoving misplaced files..." -ForegroundColor Yellow
$moveCount = 0

# Move Python scripts (except setup.py which can stay in root)
Get-ChildItem -Path . -Filter "*.py" -File | Where-Object { $_.Name -ne "setup.py" } | ForEach-Object {
    Move-Item -Path $_.FullName -Destination "utilities/python/" -Force
    Write-Host "  ? $($_.Name) ? utilities/python/" -ForegroundColor White
    $moveCount++
}

# Move PowerShell scripts to appropriate locations
Get-ChildItem -Path . -Filter "*.ps1" -File | Where-Object { $_.Name -ne "ROOT-Organize.ps1" } | ForEach-Object {
    $destination = switch -Wildcard ($_.Name) {
        "SETUP_*" { "scripts/setup/"; break }
        "COPY_*" { "scripts/utilities/"; break }
        "TEST-*" { "scripts/testing/"; break }
        "Test-*" { "scripts/testing/"; break }
        "Build-*" { "scripts/build/"; break }
        default { "scripts/"; break }
    }
    Move-Item -Path $_.FullName -Destination $destination -Force
    Write-Host "  ? $($_.Name) ? $destination" -ForegroundColor White
    $moveCount++
}

# Move JSON files (except config.json)
Get-ChildItem -Path . -Filter "*.json" -File | Where-Object { $_.Name -ne "config.json" } | ForEach-Object {
    Move-Item -Path $_.FullName -Destination "analysis/excel/" -Force
    Write-Host "  ? $($_.Name) ? analysis/excel/" -ForegroundColor White
    $moveCount++
}

# Move Excel files to examples
Get-ChildItem -Path . -Filter "*.xlsm" -File | ForEach-Object {
    Move-Item -Path $_.FullName -Destination "examples/" -Force
    Write-Host "  ? $($_.Name) ? examples/" -ForegroundColor White
    $moveCount++
}

# Move markdown files to appropriate docs/ locations
Get-ChildItem -Path . -Filter "*.md" -File | Where-Object { $allowedInRoot -notcontains $_.Name } | ForEach-Object {
    $destination = switch -Wildcard ($_.Name) {
        # Testing guides
        "*TEST_GUIDE*" { "docs/Testing/"; break }
        "*ERROR_FIX*" { "docs/Testing/"; break }
        "TEST_*" { "docs/Testing/"; break }
        
        # Getting Started
        "*QUICKSTART*" { "docs/Getting-Started/"; break }
        "*LAUNCH_PLAN*" { "docs/Getting-Started/"; break }
        "QUICK_START*" { "docs/Getting-Started/"; break }
        "GETTING_STARTED*" { "docs/Getting-Started/"; break }
        
        # Status reports
        "*SCAN_REPORT*" { "docs/Status/"; break }
        "*COMPLETE.md" { "docs/Status/"; break }
        "*STATUS*" { "docs/Status/"; break }
        "PROJECT_SCAN*" { "docs/Status/"; break }
        "*PUSH_SCAN*" { "docs/Status/"; break }
        "*ISOLATION*" { "docs/Status/"; break }
        
        # Reference docs
        "*CHECKLIST*" { "docs/Reference/"; break }
        "*REFERENCE*" { "docs/Reference/"; break }
        
        # Architecture
        "*ARCHITECTURE*" { "docs/Architecture/"; break }
        "*DESIGN*" { "docs/Architecture/"; break }
        "*ANALYSIS*" { "docs/Architecture/"; break }
        
        # Integration
        "*INTEGRATION*" { "docs/Integration/"; break }
        
        # Migration
        "*MIGRATION*" { "docs/Migration/"; break }
        
        # Default to Architecture for other docs
        default { "docs/Architecture/"; break }
    }
    
    Move-Item -Path $_.FullName -Destination $destination -Force
    Write-Host "  ? $($_.Name) ? $destination" -ForegroundColor White
    $moveCount++
}

# Move text files to analysis
Get-ChildItem -Path . -Filter "*.txt" -File | Where-Object { $_.Name -ne "requirements.txt" } | ForEach-Object {
    Move-Item -Path $_.FullName -Destination "analysis/excel/" -Force
    Write-Host "  ? $($_.Name) ? analysis/excel/" -ForegroundColor White
    $moveCount++
}

Write-Host "`n========================================" -ForegroundColor Cyan
if ($moveCount -eq 0) {
    Write-Host "   ? ROOT ALREADY CLEAN!" -ForegroundColor Green
} else {
    Write-Host "   ? MOVED $moveCount FILES" -ForegroundColor Green
}
Write-Host "========================================`n" -ForegroundColor Cyan

# Verify root is clean
Write-Host "Checking root folder..." -ForegroundColor Yellow
$rootFiles = Get-ChildItem -Path . -File | Where-Object { 
    ($_.Name -notlike ".*") -and ($allowedInRoot -notcontains $_.Name)
}

if ($rootFiles.Count -gt 0) {
    Write-Host "??  WARNING: Unexpected files still in root:" -ForegroundColor Yellow
    $rootFiles | ForEach-Object { Write-Host "  - $($_.Name)" -ForegroundColor Red }
    Write-Host "`nPlease review these files manually." -ForegroundColor Yellow
} else {
    Write-Host "? Root folder is clean!" -ForegroundColor Green
    Write-Host "`nAllowed files in root:" -ForegroundColor Cyan
    Get-ChildItem -Path . -File | Where-Object { 
        ($_.Name -notlike ".*") -and ($allowedInRoot -contains $_.Name)
    } | ForEach-Object { Write-Host "  ? $($_.Name)" -ForegroundColor White }
}

Write-Host "`nNext steps:" -ForegroundColor Yellow
Write-Host "  1. Review changes: git status" -ForegroundColor White
Write-Host "  2. Stage changes: git add ." -ForegroundColor White
Write-Host "  3. Commit: git commit -m 'chore: organize root folder'" -ForegroundColor White
Write-Host ""
