# PowerShell script to copy Hudson Certified templates with unique naming
# Adds "HUD_" prefix to all files to avoid conflicts

$scriptPath = Split-Path -Parent $MyInvocation.MyCommand.Definition
Set-Location $scriptPath

Write-Host "========================================" -ForegroundColor Cyan
Write-Host " Hudson Certified Templates Copy" -ForegroundColor Cyan
Write-Host " (with HUD_ prefix for uniqueness)" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Define source and target
$sourcePath = "C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting\Certified"
$targetPath = Join-Path $scriptPath "templates\hudson_certified"

Write-Host "Source: $sourcePath" -ForegroundColor Gray
Write-Host "Target: $targetPath" -ForegroundColor Gray
Write-Host "Prefix: HUD_ (Hudson)" -ForegroundColor Yellow
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
$categories = @{}

# Copy files with renaming
Write-Host "Copying and renaming files..." -ForegroundColor Yellow
Write-Host ""

foreach ($file in $sourceFiles) {
    try {
        # Calculate relative path
        $relativePath = $file.FullName.Substring($sourcePath.Length).TrimStart('\')
        $relativeDir = Split-Path $relativePath -Parent
        $fileName = Split-Path $relativePath -Leaf
        
        # Determine category
        $category = ($relativeDir -split '\\')[0]
        if ($category -eq "") { $category = "root" }
        
        # Count by category
        if (-not $categories.ContainsKey($category)) {
            $categories[$category] = 0
        }
        $categories[$category]++
        
        # Check if file should be renamed (SolidWorks files)
        $shouldRename = $fileName -match '^JOBNO' -and `
                       ($file.Extension -in '.SLDASM','.SLDPRT','.SLDDRW','.sldasm','.sldprt','.slddrw')
        
        if ($shouldRename) {
            # Add HUD_ prefix
            $newFileName = "HUD_" + $fileName
            $filesRenamed++
        } else {
            # Keep original name for non-JOBNO files
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
        
        # Show progress every 10 files
        if ($filesCopied % 10 -eq 0) {
            $percent = [math]::Round(($filesCopied / $totalFiles) * 100, 1)
            Write-Host "  Progress: $filesCopied/$totalFiles ($percent%)" -ForegroundColor DarkGray
        }
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
Write-Host "  Files renamed (HUD_ prefix): $filesRenamed" -ForegroundColor Yellow
if ($errors -gt 0) {
    Write-Host "  Errors: $errors" -ForegroundColor Red
}
Write-Host ""

# By category
Write-Host "Files by Category:" -ForegroundColor Cyan
foreach ($cat in $categories.Keys | Sort-Object) {
    $count = $categories[$cat]
    Write-Host "  $cat`: $count files" -ForegroundColor Gray
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
Write-Host "  templates\hudson_certified\" -ForegroundColor White
Write-Host ""
Write-Host "Naming Convention:" -ForegroundColor Yellow
Write-Host "  Original: JOBNO-7.SLDASM" -ForegroundColor Gray
Write-Host "  Copied:   HUD_JOBNO-7.SLDASM" -ForegroundColor Green
Write-Host ""
Write-Host "This prevents naming conflicts in your system!" -ForegroundColor Green
Write-Host ""

# List some examples
Write-Host "Example files copied:" -ForegroundColor Yellow
$exampleFiles = Get-ChildItem -Path $targetPath -Recurse -File -Filter "HUD_JOBNO*" | Select-Object -First 5
foreach ($example in $exampleFiles) {
    Write-Host "  $($example.Name)" -ForegroundColor Gray
}
Write-Host ""

Pause
