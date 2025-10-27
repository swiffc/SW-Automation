# Quick Root Cleanup Script
# Run from: Solidworks_Automation root folder

Write-Host "`n=== ROOT FOLDER CLEANUP ===" -ForegroundColor Cyan
Write-Host "Creating organized structure...`n" -ForegroundColor Yellow

# Create folder structure
$folders = @(
    "scripts/setup",
    "scripts/utilities",
    "utilities/python",
    "analysis/excel",
    "examples"
)

foreach ($folder in $folders) {
    if (-not (Test-Path $folder)) {
        New-Item -ItemType Directory -Path $folder -Force | Out-Null
        Write-Host "Created: $folder" -ForegroundColor Green
    }
}

Write-Host "`nMoving files..." -ForegroundColor Yellow

# Move Python scripts
Move-Item -Path "*.py" -Destination "utilities/python/" -ErrorAction SilentlyContinue
Write-Host "Moved Python scripts to utilities/python/" -ForegroundColor Green

# Move PowerShell setup scripts
Move-Item -Path "SETUP_*.ps1" -Destination "scripts/setup/" -ErrorAction SilentlyContinue
Move-Item -Path "setup_python.ps1" -Destination "scripts/setup/" -ErrorAction SilentlyContinue
Write-Host "Moved setup scripts to scripts/setup/" -ForegroundColor Green

# Move PowerShell utility scripts
Move-Item -Path "COPY_*.ps1" -Destination "scripts/utilities/" -ErrorAction SilentlyContinue
Write-Host "Moved copy scripts to scripts/utilities/" -ForegroundColor Green

# Move Excel analysis files
Move-Item -Path "*CELL_MAPPING*" -Destination "analysis/excel/" -ErrorAction SilentlyContinue
Move-Item -Path "*COMPLETE_EXCEL*" -Destination "analysis/excel/" -ErrorAction SilentlyContinue
Move-Item -Path "excel_scan_results.txt" -Destination "analysis/excel/" -ErrorAction SilentlyContinue
Move-Item -Path "complete_excel_analysis.json" -Destination "analysis/excel/" -ErrorAction SilentlyContinue
Write-Host "Moved analysis files to analysis/excel/" -ForegroundColor Green

# Move sample Excel file
Move-Item -Path "S25140-Prego1.xlsm" -Destination "examples/" -ErrorAction SilentlyContinue
Write-Host "Moved sample file to examples/" -ForegroundColor Green

# Move documentation to proper location
Move-Item -Path "BUNDLE_DUAL_APPROACH_ANALYSIS.md" -Destination "macros/csharp/Solidworks-Automation/docs/Architecture/" -ErrorAction SilentlyContinue
Move-Item -Path "PROJECT_SCAN_REPORT_*.md" -Destination "macros/csharp/Solidworks-Automation/docs/Architecture/" -ErrorAction SilentlyContinue
Write-Host "Moved documentation to docs/Architecture/" -ForegroundColor Green

Write-Host "`n=== CLEANUP COMPLETE ===" -ForegroundColor Green
Write-Host "`nYour root folder is now clean!`n" -ForegroundColor Cyan
Write-Host "Check git status: git status`n" -ForegroundColor White


