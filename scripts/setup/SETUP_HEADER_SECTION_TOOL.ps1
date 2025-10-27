# PowerShell script to set up Header Section Tool integration
# This script creates symbolic links and sets up the folder structure
# MUST BE RUN AS ADMINISTRATOR

# Ensure script is run with Administrator privileges
if (-not ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
    Write-Host "========================================" -ForegroundColor Red
    Write-Host "ERROR: Administrator privileges required" -ForegroundColor Red
    Write-Host "========================================" -ForegroundColor Red
    Write-Host ""
    Write-Host "Please right-click this script and select 'Run as Administrator'" -ForegroundColor Yellow
    Write-Host ""
    Pause
    exit 1
}

$scriptPath = Split-Path -Parent $MyInvocation.MyCommand.Definition
Set-Location $scriptPath

Write-Host "================================================" -ForegroundColor Cyan
Write-Host " Header Section Tool Integration Setup" -ForegroundColor Cyan
Write-Host "================================================" -ForegroundColor Cyan
Write-Host ""

# Load configuration
Write-Host "[1/5] Loading configuration..." -ForegroundColor Yellow
$config = Get-Content -Raw -Path "config.json" | ConvertFrom-Json

$sourcePath = $config.HeaderSectionTool.SourcePath
$targetPath = Join-Path $scriptPath $config.HeaderSectionTool.ProjectPath
$outputBasePath = Join-Path $scriptPath $config.HeaderSectionTool.OutputPath

Write-Host "  Source: $sourcePath" -ForegroundColor Gray
Write-Host "  Target: $targetPath" -ForegroundColor Gray
Write-Host "  Output: $outputBasePath" -ForegroundColor Gray
Write-Host ""

# Verify source exists
Write-Host "[2/5] Verifying source path..." -ForegroundColor Yellow
if (-not (Test-Path $sourcePath)) {
    Write-Host "  ERROR: Source path not found!" -ForegroundColor Red
    Write-Host "  Path: $sourcePath" -ForegroundColor Red
    Write-Host ""
    Write-Host "  Please verify:" -ForegroundColor Yellow
    Write-Host "  1. The path exists" -ForegroundColor Yellow
    Write-Host "  2. You have access to C:\AXC_VAULT" -ForegroundColor Yellow
    Write-Host "  3. The folder structure is correct" -ForegroundColor Yellow
    Write-Host ""
    Pause
    exit 1
}
Write-Host "  Source path verified!" -ForegroundColor Green
Write-Host ""

# Count files in source
$sourceFiles = Get-ChildItem -Path $sourcePath -Recurse -File
Write-Host "  Found $($sourceFiles.Count) files in source" -ForegroundColor Gray
Write-Host ""

# Create symbolic link
Write-Host "[3/5] Creating symbolic link..." -ForegroundColor Yellow
if (Test-Path $targetPath) {
    Write-Host "  Target already exists: $targetPath" -ForegroundColor Yellow
    
    # Check if it's already a symbolic link
    $item = Get-Item $targetPath
    if ($item.LinkType -eq "SymbolicLink") {
        Write-Host "  Existing symbolic link found" -ForegroundColor Gray
        Write-Host "  Target: $($item.Target)" -ForegroundColor Gray
        
        if ($item.Target -eq $sourcePath) {
            Write-Host "  Symbolic link is already correct!" -ForegroundColor Green
        } else {
            Write-Host "  Warning: Symbolic link points to different location" -ForegroundColor Yellow
            Write-Host "  Remove it manually if you want to recreate" -ForegroundColor Yellow
        }
    } else {
        Write-Host "  Warning: Path exists but is not a symbolic link" -ForegroundColor Yellow
        Write-Host "  Remove it manually if you want to create symbolic link" -ForegroundColor Yellow
    }
} else {
    # Ensure parent directory exists
    $parentDir = Split-Path $targetPath -Parent
    if (-not (Test-Path $parentDir)) {
        New-Item -Path $parentDir -ItemType Directory -Force | Out-Null
        Write-Host "  Created parent directory: $parentDir" -ForegroundColor Gray
    }
    
    # Create the symbolic link
    try {
        New-Item -ItemType SymbolicLink -Path $targetPath -Target $sourcePath -Force | Out-Null
        Write-Host "  Symbolic link created successfully!" -ForegroundColor Green
        Write-Host "  $targetPath -> $sourcePath" -ForegroundColor Gray
    } catch {
        Write-Host "  ERROR: Failed to create symbolic link" -ForegroundColor Red
        Write-Host "  $_" -ForegroundColor Red
        Pause
        exit 1
    }
}
Write-Host ""

# Create output directories
Write-Host "[4/5] Creating output directories..." -ForegroundColor Yellow

$outputDirs = @(
    $outputBasePath,
    (Join-Path $outputBasePath "combined"),
    (Join-Path $outputBasePath "single"),
    (Join-Path $outputBasePath "hailguard"),
    (Join-Path $outputBasePath "steamcoil"),
    (Join-Path $outputBasePath "specialty"),
    (Join-Path $outputBasePath "weldmaps")
)

foreach ($dir in $outputDirs) {
    if (-not (Test-Path $dir)) {
        New-Item -Path $dir -ItemType Directory -Force | Out-Null
        Write-Host "  Created: $dir" -ForegroundColor Gray
    } else {
        Write-Host "  Exists: $dir" -ForegroundColor DarkGray
    }
}
Write-Host "  All output directories ready!" -ForegroundColor Green
Write-Host ""

# Verify setup
Write-Host "[5/5] Verifying setup..." -ForegroundColor Yellow

$verificationPassed = $true

# Check symbolic link
if (Test-Path $targetPath) {
    $item = Get-Item $targetPath
    if ($item.LinkType -eq "SymbolicLink") {
        Write-Host "  Symbolic link: OK" -ForegroundColor Green
    } else {
        Write-Host "  Symbolic link: WARNING (not a link)" -ForegroundColor Yellow
        $verificationPassed = $false
    }
} else {
    Write-Host "  Symbolic link: FAILED" -ForegroundColor Red
    $verificationPassed = $false
}

# Check key files
$keyFiles = @(
    "Combined_\Drafting\Headers\000000_S01c-HCS.xlsx",
    "Combined_\Drafting\Headers\000000_S01c-Header.SLDASM",
    "Single_\Drafting\Headers\000000_S03-HCS.xlsx",
    "Single_\Drafting\Headers\000000_S03-Header.SLDASM"
)

$filesFound = 0
foreach ($file in $keyFiles) {
    $fullPath = Join-Path $targetPath $file
    if (Test-Path $fullPath) {
        $filesFound++
    }
}

if ($filesFound -eq $keyFiles.Count) {
    Write-Host "  Key files accessible: OK ($filesFound/$($keyFiles.Count))" -ForegroundColor Green
} else {
    Write-Host "  Key files accessible: WARNING ($filesFound/$($keyFiles.Count))" -ForegroundColor Yellow
    $verificationPassed = $false
}

# Check output directories
$outputDirsExist = $true
foreach ($dir in $outputDirs) {
    if (-not (Test-Path $dir)) {
        $outputDirsExist = $false
        break
    }
}

if ($outputDirsExist) {
    Write-Host "  Output directories: OK" -ForegroundColor Green
} else {
    Write-Host "  Output directories: FAILED" -ForegroundColor Red
    $verificationPassed = $false
}

Write-Host ""

# Summary
Write-Host "================================================" -ForegroundColor Cyan
if ($verificationPassed) {
    Write-Host " Setup Complete!" -ForegroundColor Green
    Write-Host "================================================" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "Header Section Tool successfully integrated!" -ForegroundColor Green
    Write-Host ""
    Write-Host "Next Steps:" -ForegroundColor Yellow
    Write-Host "  1. Review HEADER_SECTION_TOOL_INTEGRATION.md" -ForegroundColor White
    Write-Host "  2. Open Visual Studio and build the solution" -ForegroundColor White
    Write-Host "  3. Test with a sample header configuration" -ForegroundColor White
    Write-Host ""
    Write-Host "Files accessible at:" -ForegroundColor Yellow
    Write-Host "  $targetPath" -ForegroundColor White
    Write-Host ""
    Write-Host "Output will be saved to:" -ForegroundColor Yellow
    Write-Host "  $outputBasePath" -ForegroundColor White
} else {
    Write-Host " Setup Complete with Warnings" -ForegroundColor Yellow
    Write-Host "================================================" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "Some verification checks failed." -ForegroundColor Yellow
    Write-Host "Please review the warnings above." -ForegroundColor Yellow
    Write-Host ""
    Write-Host "If files are not accessible:" -ForegroundColor Yellow
    Write-Host "  1. Verify you have access to C:\AXC_VAULT" -ForegroundColor White
    Write-Host "  2. Check the source path exists" -ForegroundColor White
    Write-Host "  3. Ensure symbolic link was created properly" -ForegroundColor White
}

Write-Host ""
Write-Host "================================================" -ForegroundColor Cyan
Write-Host ""

Pause

