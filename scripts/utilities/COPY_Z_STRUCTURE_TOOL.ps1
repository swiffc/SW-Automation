# PowerShell script to copy Z Structure Tool
# Adds "ZST_" prefix to Z_ files to avoid conflicts

$scriptPath = Split-Path -Parent $MyInvocation.MyCommand.Definition
Set-Location $scriptPath

Write-Host "========================================" -ForegroundColor Cyan
Write-Host " Z Structure Tool Copy" -ForegroundColor Cyan
Write-Host " (with ZST_ prefix for uniqueness)" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Define source and target
$sourcePath = "C:\AXC_VAULT\Active\_Automation Tools\Z Structure Tool"
$targetPath = Join-Path $scriptPath "templates\z_structure_tool"

Write-Host "Source: $sourcePath" -ForegroundColor Gray
Write-Host "Target: $targetPath" -ForegroundColor Gray
Write-Host "Prefix: ZST_ (Z Structure Tool)" -ForegroundColor Yellow
Write-Host ""

# Check if source exists
if (-not (Test-Path $sourcePath)) {
    Write-Host "ERROR: Source path not found!" -ForegroundColor Red
    Write-Host "Path: $sourcePath" -ForegroundColor Red
    Pause
    exit 1
}

# Create target directory
if (-not (Test-Path $targetPath)) {
    New-Item -Path $targetPath -ItemType Directory -Force | Out-Null
    Write-Host "Created target directory" -ForegroundColor Green
} else {
    Write-Host "Target directory already exists" -ForegroundColor DarkGray
}

Write-Host ""

# Count files first
Write-Host "Counting files..." -ForegroundColor Gray
$sourceFiles = Get-ChildItem -Path $sourcePath -Recurse -File
$totalFiles = $sourceFiles.Count
Write-Host "Found $totalFiles files to copy" -ForegroundColor Cyan
Write-Host ""

# Statistics
$filesCopied = 0
$filesRenamed = 0
$errors = 0

# Copy files with renaming
Write-Host "Copying and renaming files..." -ForegroundColor Yellow
Write-Host ""

foreach ($file in $sourceFiles) {
    try {
        # Calculate relative path
        $relativePath = $file.FullName.Substring($sourcePath.Length).TrimStart('\')
        $relativeDir = Split-Path $relativePath -Parent
        $fileName = Split-Path $relativePath -Leaf
        
        # Check if file should be renamed (starts with Z_ and is a CAD file)
        $shouldRename = $fileName -match '^Z_' -and `
                       ($file.Extension -in '.SLDASM','.SLDPRT','.SLDDRW','.sldasm','.sldprt','.slddrw')
        
        if ($shouldRename) {
            # Add ZST_ prefix (keep the Z_)
            $newFileName = "ZST_" + $fileName
            $filesRenamed++
            Write-Host "  Renaming: $fileName -> $newFileName" -ForegroundColor Yellow
        } else {
            # Keep original name
            $newFileName = $fileName
        }
        
        # Create target path properly
        if ($relativeDir) {
            $targetDir = Join-Path $targetPath $relativeDir
            $targetFile = Join-Path $targetDir $newFileName
        } else {
            $targetDir = $targetPath
            $targetFile = Join-Path $targetPath $newFileName
        }
        
        # Create target directory if needed
        if (-not (Test-Path $targetDir)) {
            New-Item -Path $targetDir -ItemType Directory -Force | Out-Null
        }
        
        # Copy file
        Copy-Item -Path $file.FullName -Destination $targetFile -Force
        $filesCopied++
        
        Write-Host "  Copied: $fileName" -ForegroundColor Gray
    }
    catch {
        Write-Host "  ERROR copying: $($file.Name)" -ForegroundColor Red
        Write-Host "    $($_.Exception.Message)" -ForegroundColor Red
        $errors++
    }
}

Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host " Copy Complete!" -ForegroundColor Green
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Summary
Write-Host "Summary:" -ForegroundColor Cyan
Write-Host "  Total files copied: $filesCopied" -ForegroundColor Green
Write-Host "  Files renamed (ZST_ prefix): $filesRenamed" -ForegroundColor Yellow
if ($errors -gt 0) {
    Write-Host "  Errors: $errors" -ForegroundColor Red
}
Write-Host ""

# Calculate size
if (Test-Path $targetPath) {
    $size = (Get-ChildItem -Path $targetPath -Recurse -File | Measure-Object -Property Length -Sum).Sum
    $sizeMB = [math]::Round($size / 1MB, 2)
    Write-Host "Total size: $sizeMB MB" -ForegroundColor Cyan
}

Write-Host ""
Write-Host "Files are now in:" -ForegroundColor Yellow
Write-Host "  templates\z_structure_tool\" -ForegroundColor White
Write-Host ""
Write-Host "Naming Convention:" -ForegroundColor Yellow
Write-Host "  Original: Z_Hardware-01.SLDASM" -ForegroundColor Gray
Write-Host "  Copied:   ZST_Z_Hardware-01.SLDASM" -ForegroundColor Green
Write-Host ""

# List all copied files
Write-Host "All files copied:" -ForegroundColor Yellow
$copiedFiles = Get-ChildItem -Path $targetPath -Recurse -File
foreach ($copiedFile in $copiedFiles) {
    $relativePath = $copiedFile.FullName.Substring($targetPath.Length).TrimStart('\')
    Write-Host "  $relativePath" -ForegroundColor Gray
}
Write-Host ""

Pause

