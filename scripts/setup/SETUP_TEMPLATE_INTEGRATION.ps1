# SolidWorks Automation - Template Integration Setup
# Run this script as Administrator

Write-Host "========================================" -ForegroundColor Cyan
Write-Host " SolidWorks Automation Template Setup" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Check if running as Administrator
$isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

if (-not $isAdmin) {
    Write-Host "ERROR: This script must be run as Administrator!" -ForegroundColor Red
    Write-Host "Right-click PowerShell and select 'Run as Administrator'" -ForegroundColor Yellow
    pause
    exit 1
}

# Paths
$ProjectRoot = "C:\Users\DCornealius\CascadeProjects\Solidworks_Automation"
$VaultPath = "C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting\Certified"
$TemplatesPath = Join-Path $ProjectRoot "templates"
$CertifiedPath = Join-Path $TemplatesPath "certified"
$OutputPath = Join-Path $ProjectRoot "output"

Write-Host "Project Root: $ProjectRoot" -ForegroundColor Gray
Write-Host "Vault Path: $VaultPath" -ForegroundColor Gray
Write-Host ""

# Step 1: Verify Vault Path Exists
Write-Host "[1/6] Verifying AXC_VAULT access..." -ForegroundColor Yellow

if (-not (Test-Path $VaultPath)) {
    Write-Host "ERROR: Cannot access AXC_VAULT at:" -ForegroundColor Red
    Write-Host "  $VaultPath" -ForegroundColor Red
    Write-Host ""
    Write-Host "Please ensure:" -ForegroundColor Yellow
    Write-Host "  1. You have network access to AXC_VAULT" -ForegroundColor Yellow
    Write-Host "  2. The path is correct" -ForegroundColor Yellow
    Write-Host "  3. You have read permissions" -ForegroundColor Yellow
    pause
    exit 1
}

Write-Host "  ? Vault accessible" -ForegroundColor Green
Write-Host ""

# Step 2: Create Templates Directory
Write-Host "[2/6] Creating templates directory..." -ForegroundColor Yellow

if (-not (Test-Path $TemplatesPath)) {
    New-Item -ItemType Directory -Path $TemplatesPath -Force | Out-Null
    Write-Host "  ? Created templates directory" -ForegroundColor Green
} else {
    Write-Host "  ? Templates directory exists" -ForegroundColor Green
}
Write-Host ""

# Step 3: Create or Update Symbolic Link
Write-Host "[3/6] Creating symbolic link to certified templates..." -ForegroundColor Yellow

if (Test-Path $CertifiedPath) {
    # Check if it's already a symbolic link
    $item = Get-Item $CertifiedPath
    if ($item.Attributes -band [System.IO.FileAttributes]::ReparsePoint) {
        Write-Host "  ? Symbolic link already exists" -ForegroundColor Green
    } else {
        Write-Host "  ! Path exists but is not a symbolic link" -ForegroundColor Yellow
        Write-Host "    Renaming existing directory..." -ForegroundColor Yellow
        Rename-Item $CertifiedPath "$CertifiedPath.backup.$(Get-Date -Format 'yyyyMMdd_HHmmss')" -Force
        
        Write-Host "    Creating symbolic link..." -ForegroundColor Yellow
        New-Item -ItemType SymbolicLink -Path $CertifiedPath -Target $VaultPath -Force | Out-Null
        Write-Host "  ? Symbolic link created" -ForegroundColor Green
    }
} else {
    New-Item -ItemType SymbolicLink -Path $CertifiedPath -Target $VaultPath -Force | Out-Null
    Write-Host "  ? Symbolic link created" -ForegroundColor Green
}
Write-Host ""

# Step 4: Verify Link Works
Write-Host "[4/6] Verifying symbolic link..." -ForegroundColor Yellow

$testFile = Get-ChildItem $CertifiedPath -File -ErrorAction SilentlyContinue | Select-Object -First 1

if ($testFile) {
    Write-Host "  ? Can read files through symbolic link" -ForegroundColor Green
    Write-Host "    Sample file: $($testFile.Name)" -ForegroundColor Gray
} else {
    Write-Host "  ! Warning: No files found through symbolic link" -ForegroundColor Yellow
}
Write-Host ""

# Step 5: Create Output Directories
Write-Host "[5/6] Creating output directories..." -ForegroundColor Yellow

$outputDirs = @(
    "output",
    "output\generated_bundles",
    "output\generated_headers",
    "output\generated_hoods",
    "output\generated_machinery_mounts",
    "output\generated_plenums",
    "output\generated_structures",
    "output\generated_walkways",
    "output\exports",
    "output\exports\pdf",
    "output\exports\step",
    "output\exports\dxf",
    "logs"
)

foreach ($dir in $outputDirs) {
    $fullPath = Join-Path $ProjectRoot $dir
    if (-not (Test-Path $fullPath)) {
        New-Item -ItemType Directory -Path $fullPath -Force | Out-Null
    }
}

Write-Host "  ? Output directories created" -ForegroundColor Green
Write-Host ""

# Step 6: Verify Template Components
Write-Host "[6/6] Verifying template components..." -ForegroundColor Yellow

$components = @(
    @{Name="Bundle"; Count=21},
    @{Name="Header"; Count=17},
    @{Name="Hood"; Count=8},
    @{Name="MachineryMount"; Count=26},
    @{Name="Plenum"; Count=41},
    @{Name="Structure"; Count=27},
    @{Name="Walkway"; Count=64}
)

$allValid = $true
foreach ($component in $components) {
    $compPath = Join-Path $CertifiedPath $component.Name
    if (Test-Path $compPath) {
        $fileCount = (Get-ChildItem $compPath -File -Recurse).Count
        if ($fileCount -ge $component.Count) {
            Write-Host "  ? $($component.Name): $fileCount files" -ForegroundColor Green
        } else {
            Write-Host "  ! $($component.Name): $fileCount files (expected $($component.Count))" -ForegroundColor Yellow
            $allValid = $false
        }
    } else {
        Write-Host "  ? $($component.Name): Directory not found" -ForegroundColor Red
        $allValid = $false
    }
}

Write-Host ""

# Summary
Write-Host "========================================" -ForegroundColor Cyan
Write-Host " Setup Complete!" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

if ($allValid) {
    Write-Host "? All components verified successfully!" -ForegroundColor Green
} else {
    Write-Host "! Some components have warnings" -ForegroundColor Yellow
    Write-Host "  Review the output above" -ForegroundColor Yellow
}

Write-Host ""
Write-Host "Template Path: $CertifiedPath" -ForegroundColor Gray
Write-Host "Output Path:   $OutputPath" -ForegroundColor Gray
Write-Host ""

Write-Host "Next Steps:" -ForegroundColor Yellow
Write-Host "  1. Open Visual Studio as Administrator" -ForegroundColor White
Write-Host "  2. Build your Solidworks-Automation solution" -ForegroundColor White
Write-Host "  3. Load SolidWorks and test the add-in" -ForegroundColor White
Write-Host "  4. Verify templates are accessible from the add-in" -ForegroundColor White
Write-Host ""

Write-Host "Press any key to exit..."
$null = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")

