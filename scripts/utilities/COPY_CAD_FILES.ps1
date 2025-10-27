# PowerShell script to copy all CAD files to project templates
# This creates a local copy of all automation tool files

$scriptPath = Split-Path -Parent $MyInvocation.MyCommand.Definition
Set-Location $scriptPath

Write-Host "========================================" -ForegroundColor Cyan
Write-Host " CAD Files Copy to Templates" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Define source and target paths
$sources = @{
    "HeaderSectionTool" = @{
        "SourcePath" = "C:\AXC_VAULT\Active\_Automation Tools\Header Section Tool"
        "TargetPath" = "templates\header_section_tool"
        "Description" = "Header Section Tool (125 files)"
    }
    "XCHStructureTool" = @{
        "SourcePath" = "C:\AXC_VAULT\Active\_Automation Tools\XCH Structure Tool"
        "TargetPath" = "templates\xch_structure_tool"
        "Description" = "XCH Structure Tool (311 files)"
    }
}

$totalFilesCopied = 0
$totalErrors = 0

foreach ($toolName in $sources.Keys) {
    $tool = $sources[$toolName]
    $sourcePath = $tool.SourcePath
    $targetPath = Join-Path $scriptPath $tool.TargetPath
    $description = $tool.Description
    
    Write-Host "[$toolName] $description" -ForegroundColor Yellow
    Write-Host "  Source: $sourcePath" -ForegroundColor Gray
    Write-Host "  Target: $targetPath" -ForegroundColor Gray
    Write-Host ""
    
    # Check if source exists
    if (-not (Test-Path $sourcePath)) {
        Write-Host "  ERROR: Source path not found!" -ForegroundColor Red
        Write-Host "  Skipping $toolName" -ForegroundColor Yellow
        Write-Host ""
        $totalErrors++
        continue
    }
    
    # Create target directory if it doesn't exist
    if (-not (Test-Path $targetPath)) {
        New-Item -Path $targetPath -ItemType Directory -Force | Out-Null
        Write-Host "  Created target directory" -ForegroundColor Green
    } else {
        Write-Host "  Target directory already exists" -ForegroundColor DarkGray
    }
    
    # Count files first
    Write-Host "  Counting files..." -ForegroundColor Gray
    $sourceFiles = Get-ChildItem -Path $sourcePath -Recurse -File
    $totalFiles = $sourceFiles.Count
    Write-Host "  Found $totalFiles files to copy" -ForegroundColor Cyan
    Write-Host ""
    
    # Copy files with progress
    Write-Host "  Copying files..." -ForegroundColor Yellow
    $filesCopied = 0
    $filesSkipped = 0
    $errors = 0
    
    foreach ($file in $sourceFiles) {
        try {
            # Calculate relative path
            $relativePath = $file.FullName.Substring($sourcePath.Length).TrimStart('\')
            $targetFile = Join-Path $targetPath $relativePath
            $targetDir = Split-Path $targetFile -Parent
            
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
                Write-Host "    Progress: $filesCopied/$totalFiles ($percent%)" -ForegroundColor DarkGray
            }
        }
        catch {
            Write-Host "    ERROR copying: $($file.Name)" -ForegroundColor Red
            $errors++
        }
    }
    
    Write-Host ""
    Write-Host "  Summary:" -ForegroundColor Cyan
    Write-Host "    Files copied: $filesCopied" -ForegroundColor Green
    if ($filesSkipped -gt 0) {
        Write-Host "    Files skipped: $filesSkipped" -ForegroundColor Yellow
    }
    if ($errors -gt 0) {
        Write-Host "    Errors: $errors" -ForegroundColor Red
    }
    Write-Host ""
    
    $totalFilesCopied += $filesCopied
    $totalErrors += $errors
}

# Final summary
Write-Host "========================================" -ForegroundColor Cyan
Write-Host " Copy Complete!" -ForegroundColor Green
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Total files copied: $totalFilesCopied" -ForegroundColor Green
if ($totalErrors -gt 0) {
    Write-Host "Total errors: $totalErrors" -ForegroundColor Red
}
Write-Host ""

# Show directory sizes
Write-Host "Directory Sizes:" -ForegroundColor Cyan
foreach ($toolName in $sources.Keys) {
    $tool = $sources[$toolName]
    $targetPath = Join-Path $scriptPath $tool.TargetPath
    
    if (Test-Path $targetPath) {
        $size = (Get-ChildItem -Path $targetPath -Recurse -File | Measure-Object -Property Length -Sum).Sum
        $sizeMB = [math]::Round($size / 1MB, 2)
        Write-Host "  $toolName`: $sizeMB MB" -ForegroundColor Gray
    }
}

Write-Host ""
Write-Host "Files are now in:" -ForegroundColor Yellow
Write-Host "  templates\header_section_tool\" -ForegroundColor White
Write-Host "  templates\xch_structure_tool\" -ForegroundColor White
Write-Host ""

Pause

