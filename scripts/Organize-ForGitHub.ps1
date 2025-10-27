# SolidWorks Automation - GitHub Organization Script
# Purpose: Clean and organize project structure before GitHub commit
# Date: October 27, 2025

param(
    [switch]$WhatIf = $false,
    [switch]$Force = $false
)

$ErrorActionPreference = "Stop"
$RootPath = Split-Path -Parent (Split-Path -Parent $PSScriptRoot)

Write-Host "`n========================================" -ForegroundColor Cyan
Write-Host "  SolidWorks Automation - GitHub Prep" -ForegroundColor Cyan
Write-Host "========================================`n" -ForegroundColor Cyan

if ($WhatIf) {
    Write-Host "RUNNING IN PREVIEW MODE (No changes will be made)`n" -ForegroundColor Yellow
}

# Function to move file safely
function Move-FileToFolder {
    param(
        [string]$SourceFile,
        [string]$DestinationFolder,
        [switch]$WhatIf
    )
    
    if (Test-Path $SourceFile) {
        $fileName = Split-Path -Leaf $SourceFile
        $destPath = Join-Path $DestinationFolder $fileName
        
        # Create destination folder if it doesn't exist
        if (-not (Test-Path $DestinationFolder)) {
            if ($WhatIf) {
                Write-Host "  [PREVIEW] Would create folder: $DestinationFolder" -ForegroundColor DarkGray
            } else {
                New-Item -ItemType Directory -Path $DestinationFolder -Force | Out-Null
                Write-Host "  Created folder: $DestinationFolder" -ForegroundColor Green
            }
        }
        
        # Move file
        if ($WhatIf) {
            Write-Host "  [PREVIEW] Would move: $fileName -> $DestinationFolder" -ForegroundColor DarkGray
        } else {
            Move-Item -Path $SourceFile -Destination $destPath -Force
            Write-Host "  Moved: $fileName" -ForegroundColor Green
        }
        return $true
    }
    return $false
}

# Step 1: Create documentation folder structure
Write-Host "STEP 1: Creating documentation folders...`n" -ForegroundColor Yellow

$docFolders = @(
    "docs/Getting-Started",
    "docs/Integration",
    "docs/Architecture",
    "docs/Testing",
    "docs/Migration",
    "docs/Reference",
    "docs/Status"
)

foreach ($folder in $docFolders) {
    $fullPath = Join-Path $RootPath $folder
    if (-not (Test-Path $fullPath)) {
        if ($WhatIf) {
            Write-Host "  [PREVIEW] Would create: $folder" -ForegroundColor DarkGray
        } else {
            New-Item -ItemType Directory -Path $fullPath -Force | Out-Null
            Write-Host "  Created: $folder" -ForegroundColor Green
        }
    } else {
        Write-Host "  Exists: $folder" -ForegroundColor Gray
    }
}

# Step 2: Organize documentation files
Write-Host "`nSTEP 2: Organizing documentation files...`n" -ForegroundColor Yellow

# Files to move to Getting-Started
$gettingStartedFiles = @(
    "GETTING_STARTED.md",
    "README_START_HERE.md"
)

foreach ($file in $gettingStartedFiles) {
    $sourcePath = Join-Path $RootPath $file
    $destFolder = Join-Path $RootPath "docs/Getting-Started"
    Move-FileToFolder -SourceFile $sourcePath -DestinationFolder $destFolder -WhatIf:$WhatIf
}

# Files to move to Integration
$integrationFiles = @(
    "UNIFIEDUI_BUNDLE_INTEGRATION_GUIDE.md",
    "INTEGRATION_COMPLETE_SUMMARY.md",
    "SESSION_COMPLETION_REPORT.md"
)

foreach ($file in $integrationFiles) {
    $sourcePath = Join-Path $RootPath $file
    $destFolder = Join-Path $RootPath "docs/Integration"
    Move-FileToFolder -SourceFile $sourcePath -DestinationFolder $destFolder -WhatIf:$WhatIf
}

# Files to move to Architecture
$architectureFiles = @(
    "PROJECT_SCAN_REPORT.md",
    "REPOSITORY_ANALYSIS.md",
    "RESCAN_RESULTS.md"
)

foreach ($file in $architectureFiles) {
    $sourcePath = Join-Path $RootPath $file
    $destFolder = Join-Path $RootPath "docs/Architecture"
    Move-FileToFolder -SourceFile $sourcePath -DestinationFolder $destFolder -WhatIf:$WhatIf
}

# Files to move to Reference (or delete)
$referenceFiles = @(
    "APPLY_FIXES.md",
    "FIXES_APPLIED.md"
)

foreach ($file in $referenceFiles) {
    $sourcePath = Join-Path $RootPath $file
    $destFolder = Join-Path $RootPath "docs/Reference"
    Move-FileToFolder -SourceFile $sourcePath -DestinationFolder $destFolder -WhatIf:$WhatIf
}

# Step 3: Delete temporary files
Write-Host "`nSTEP 3: Cleaning temporary files...`n" -ForegroundColor Yellow

$tempFiles = @(
    "scan_complete.txt",
    "BUILD_STATUS.txt"
)

foreach ($file in $tempFiles) {
    $filePath = Join-Path $RootPath $file
    if (Test-Path $filePath) {
        if ($WhatIf) {
            Write-Host "  [PREVIEW] Would delete: $file" -ForegroundColor DarkGray
        } else {
            Remove-Item -Path $filePath -Force
            Write-Host "  Deleted: $file" -ForegroundColor Green
        }
    }
}

# Step 4: Clean build artifacts
Write-Host "`nSTEP 4: Cleaning build artifacts...`n" -ForegroundColor Yellow

$artifactFolders = @(
    ".vs",
    "*/bin",
    "*/obj",
    "packages/*/lib",
    "packages/*/build"
)

$artifactCount = 0
foreach ($pattern in $artifactFolders) {
    $folders = Get-ChildItem -Path $RootPath -Directory -Recurse -Filter $pattern -ErrorAction SilentlyContinue
    foreach ($folder in $folders) {
        if ($WhatIf) {
            Write-Host "  [PREVIEW] Would delete: $($folder.Name)" -ForegroundColor DarkGray
        } else {
            Remove-Item -Path $folder.FullName -Recurse -Force -ErrorAction SilentlyContinue
            Write-Host "  Deleted: $($folder.FullName.Replace($RootPath, '.'))" -ForegroundColor Green
        }
        $artifactCount++
    }
}

if ($artifactCount -eq 0) {
    Write-Host "  No build artifacts found" -ForegroundColor Gray
}

# Step 5: Verify .gitignore
Write-Host "`nSTEP 5: Verifying .gitignore...`n" -ForegroundColor Yellow

$gitignorePath = Join-Path $RootPath ".gitignore"
if (Test-Path $gitignorePath) {
    Write-Host "  .gitignore exists: " -NoNewline
    Write-Host "OK" -ForegroundColor Green
} else {
    Write-Host "  .gitignore: " -NoNewline
    Write-Host "MISSING - Please run script again" -ForegroundColor Red
}

# Step 6: Summary
Write-Host "`n========================================" -ForegroundColor Cyan
Write-Host "  SUMMARY" -ForegroundColor Cyan
Write-Host "========================================`n" -ForegroundColor Cyan

if ($WhatIf) {
    Write-Host "This was a PREVIEW. No changes were made." -ForegroundColor Yellow
    Write-Host "Run without -WhatIf to apply changes:`n" -ForegroundColor Yellow
    Write-Host "  .\scripts\Organize-ForGitHub.ps1`n" -ForegroundColor White
} else {
    Write-Host "Organization complete!" -ForegroundColor Green
    Write-Host "`nNext steps:" -ForegroundColor Cyan
    Write-Host "  1. Review organized structure" -ForegroundColor White
    Write-Host "  2. Check git status: git status" -ForegroundColor White
    Write-Host "  3. Add files: git add ." -ForegroundColor White
    Write-Host "  4. Commit: git commit -m 'Initial organized commit'" -ForegroundColor White
    Write-Host "  5. Push to GitHub: git push origin main`n" -ForegroundColor White
}

Write-Host "Documentation organized in:" -ForegroundColor Cyan
Write-Host "  docs/Getting-Started/" -ForegroundColor White
Write-Host "  docs/Integration/" -ForegroundColor White
Write-Host "  docs/Architecture/" -ForegroundColor White
Write-Host "  docs/Testing/" -ForegroundColor White
Write-Host "  docs/Reference/" -ForegroundColor White
Write-Host "  docs/Status/`n" -ForegroundColor White

# Step 7: Generate file count report
Write-Host "File counts:" -ForegroundColor Cyan
$sourceFiles = (Get-ChildItem -Path $RootPath -Recurse -File -Include "*.cs","*.xaml" -Exclude "*AssemblyInfo.cs" | Measure-Object).Count
$docFiles = (Get-ChildItem -Path (Join-Path $RootPath "docs") -Recurse -File -Filter "*.md" -ErrorAction SilentlyContinue | Measure-Object).Count
$templateFiles = (Get-ChildItem -Path (Join-Path $RootPath "../../../templates") -Recurse -File -ErrorAction SilentlyContinue | Measure-Object).Count

Write-Host "  Source files (.cs, .xaml): $sourceFiles" -ForegroundColor White
Write-Host "  Documentation files (.md): $docFiles" -ForegroundColor White
Write-Host "  Template files: $templateFiles" -ForegroundColor White

Write-Host "`nReady for GitHub commit!`n" -ForegroundColor Green

