# SolidWorks Automation - Python Setup Script
# Run this script to set up Python environment

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "SolidWorks Python Environment Setup" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Check if Python is installed
Write-Host "[1/5] Checking Python installation..." -ForegroundColor Yellow
try {
    $pythonVersion = python --version 2>&1
    Write-Host "  ? Found: $pythonVersion" -ForegroundColor Green
} catch {
    Write-Host "  ? Python not found!" -ForegroundColor Red
    Write-Host "  ? Please install Python from https://www.python.org/downloads/" -ForegroundColor Yellow
    Write-Host "  ? Make sure to check 'Add Python to PATH' during installation" -ForegroundColor Yellow
    exit 1
}

# Create virtual environment
Write-Host ""
Write-Host "[2/5] Creating virtual environment..." -ForegroundColor Yellow
if (Test-Path "venv") {
    Write-Host "  ? Virtual environment already exists" -ForegroundColor Yellow
    $response = Read-Host "  Delete and recreate? (y/n)"
    if ($response -eq 'y') {
        Remove-Item -Recurse -Force "venv"
        python -m venv venv
        Write-Host "  ? Virtual environment recreated" -ForegroundColor Green
    } else {
        Write-Host "  ? Skipping recreation" -ForegroundColor Yellow
    }
} else {
    python -m venv venv
    Write-Host "  ? Virtual environment created" -ForegroundColor Green
}

# Activate virtual environment
Write-Host ""
Write-Host "[3/5] Activating virtual environment..." -ForegroundColor Yellow
& ".\venv\Scripts\Activate.ps1"
Write-Host "  ? Virtual environment activated" -ForegroundColor Green

# Upgrade pip
Write-Host ""
Write-Host "[4/5] Upgrading pip..." -ForegroundColor Yellow
python -m pip install --upgrade pip --quiet
Write-Host "  ? pip upgraded" -ForegroundColor Green

# Install requirements
Write-Host ""
Write-Host "[5/5] Installing Python packages..." -ForegroundColor Yellow
if (Test-Path "requirements.txt") {
    pip install -r requirements.txt
    Write-Host "  ? Packages installed successfully" -ForegroundColor Green
} else {
    Write-Host "  ? requirements.txt not found!" -ForegroundColor Red
    exit 1
}

# Run test script
Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Setup Complete!" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Next steps:" -ForegroundColor Yellow
Write-Host "  1. Make sure SolidWorks is running" -ForegroundColor White
Write-Host "  2. Run: python test_connection.py" -ForegroundColor White
Write-Host ""
Write-Host "To activate environment in future sessions:" -ForegroundColor Yellow
Write-Host "  .\venv\Scripts\Activate.ps1" -ForegroundColor White
Write-Host ""

# Prompt to run test
$runTest = Read-Host "Run connection test now? (y/n)"
if ($runTest -eq 'y') {
    Write-Host ""
    Write-Host "Running test..." -ForegroundColor Yellow
    Write-Host ""
    python test_connection.py
}

Write-Host ""
Write-Host "Done! Happy coding! ??" -ForegroundColor Green

