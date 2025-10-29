# Quick Setup Script for OpenAI Integration
# Run this script to set up your OpenAI API configuration

Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "  OpenAI Integration Setup" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Check if .env already exists
if (Test-Path ".env") {
    Write-Host "??  .env file already exists!" -ForegroundColor Yellow
    $overwrite = Read-Host "Do you want to overwrite it? (y/N)"
    if ($overwrite -ne "y" -and $overwrite -ne "Y") {
        Write-Host "? Setup cancelled. Edit .env manually." -ForegroundColor Red
        exit
    }
}

# Copy .env.example to .env
Write-Host "?? Creating .env file from template..." -ForegroundColor Green
Copy-Item ".env.example" ".env" -Force

Write-Host "? .env file created!" -ForegroundColor Green
Write-Host ""

# Prompt for API key
Write-Host "?? OpenAI API Key Setup" -ForegroundColor Cyan
Write-Host ""
Write-Host "IMPORTANT: The API key you shared in chat has been EXPOSED!" -ForegroundColor Red
Write-Host "You must create a NEW key:" -ForegroundColor Yellow
Write-Host "  1. Go to: https://platform.openai.com/api-keys" -ForegroundColor White
Write-Host "  2. Revoke the old key (sk-proj-MJTm...)" -ForegroundColor White
Write-Host "  3. Click '+ Create new secret key'" -ForegroundColor White
Write-Host "  4. Copy the new key and paste it below" -ForegroundColor White
Write-Host ""

$apiKey = Read-Host "Enter your NEW OpenAI API key (starts with sk-)"

if ($apiKey -match "^sk-") {
    # Update .env file
    $envContent = Get-Content ".env"
    $envContent = $envContent -replace "OPENAI_API_KEY=.*", "OPENAI_API_KEY=$apiKey"
    $envContent | Set-Content ".env"
    
    Write-Host ""
    Write-Host "? API key saved to .env file!" -ForegroundColor Green
} else {
    Write-Host ""
    Write-Host "??  Invalid API key format. It should start with 'sk-'" -ForegroundColor Yellow
    Write-Host "Please edit .env manually and add your key." -ForegroundColor Yellow
}

Write-Host ""
Write-Host "?? Installing Python dependencies..." -ForegroundColor Green
pip install -r requirements.txt 2>&1 | Out-Null

if ($LASTEXITCODE -eq 0) {
    Write-Host "? Dependencies installed!" -ForegroundColor Green
} else {
    Write-Host "??  Some dependencies may have failed. Run manually:" -ForegroundColor Yellow
    Write-Host "    pip install -r requirements.txt" -ForegroundColor White
}

Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "  Setup Complete! ??" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Try these commands:" -ForegroundColor Green
Write-Host ""
Write-Host "  # Ask the AI assistant a question:" -ForegroundColor White
Write-Host '  python utilities\python\ai_repo_assistant.py "What is this repository?"' -ForegroundColor Gray
Write-Host ""
Write-Host "  # Validate an Excel file:" -ForegroundColor White
Write-Host '  python utilities\python\ai_excel_validator.py "templates\header_section_tool\Combined_\Drafting\Headers\000000_S01c-HCS.xlsx"' -ForegroundColor Gray
Write-Host ""
Write-Host "  # Interactive mode:" -ForegroundColor White
Write-Host '  python utilities\python\ai_repo_assistant.py' -ForegroundColor Gray
Write-Host ""
Write-Host "?? Full guide: utilities\python\OPENAI_SETUP.md" -ForegroundColor Cyan
Write-Host ""
