# Fix Critical UI Issues
# 1. Remove emoji symbols (replace ?? with text)
# 2. Fix light backgrounds in Expanders
# 3. Verify tool selector

$panelsPath = "macros\csharp\Solidworks-Automation\UnifiedUI\Views"

Write-Host "`n========================================" -ForegroundColor Cyan
Write-Host "  ?? FIXING CRITICAL UI ISSUES" -ForegroundColor Yellow
Write-Host "========================================`n" -ForegroundColor Cyan

# Get all panel files
$panelFiles = Get-ChildItem $panelsPath -Filter "*.xaml" | Where-Object { $_.Name -ne "ProgressWindow.xaml" }

foreach ($file in $panelFiles) {
    Write-Host "Processing: $($file.Name)" -ForegroundColor Yellow
    
    $content = Get-Content $file.FullName -Raw
    $changed = $false
    
    # Fix 1: Remove ALL emojis (replace common ones with empty string or text equivalents)
    $originalContent = $content
    
    # Remove emoji markers
    $content = $content -replace 'ü"ß', ''          # wrench
    $content = $content -replace 'ü"ù', ''          # ruler
    $content = $content -replace 'ü"ã', ''          # clipboard
    $content = $content -replace 'ü"•', ''          # inbox
    $content = $content -replace 'ü'æ', ''          # floppy disk
    $content = $content -replace 'ü"§', ''          # outbox
    $content = $content -replace 'üéõÔ∏ù', ''       # control knobs
    $content = $content -replace '‚ú"', 'ï'         # checkmark to bullet
    $content = $content -replace 'ù', ''           # replacement character
    $content = $content -replace '??', ''
    $content = $content -replace '??', ''
    $content = $content -replace '??', ''
    $content = $content -replace '??', ''
    $content = $content -replace '??', ''
    $content = $content -replace '??', ''
    $content = $content -replace '???', ''
    $content = $content -replace '?', 'ï'
    $content = $content -replace '??', ''
    $content = $content -replace '???', ''
    
    if ($content -ne $originalContent) {
        $changed = $true
        Write-Host "  ? Removed emoji symbols" -ForegroundColor Green
    }
    
    # Fix 2: Replace light backgrounds with dark theme
    $originalContent = $content
    
    $lightBackgrounds = @(
        'Background="#FFF8E1"',
        'Background="#E3F2FD"',
        'Background="#F3E5F5"',
        'Background="#ECEFF1"',
        'Background="#EFEBE9"',
        'Background="#FAFAFA"',
        'Background="#E8F5E9"',
        'Background="White"'
    )
    
    foreach ($lightBg in $lightBackgrounds) {
        if ($content -match [regex]::Escape($lightBg)) {
            $content = $content -replace [regex]::Escape($lightBg), 'Background="{StaticResource SecondaryBackground}"'
            $changed = $true
        }
    }
    
    # Fix borders
    $content = $content -replace 'BorderBrush="#E0E0E0"', 'BorderBrush="{StaticResource BorderColor}"'
    $content = $content -replace 'BorderBrush="#[A-F0-9]{6}"', 'BorderBrush="{StaticResource BorderColor}"'
    
    if ($content -ne $originalContent) {
        Write-Host "  ? Fixed light backgrounds" -ForegroundColor Green
    }
    
    # Fix 3: Fix text colors
    $content = $content -replace 'Foreground="#666"', 'Foreground="{StaticResource SecondaryText}"'
    $content = $content -replace 'Foreground="#[0-9A-F]{6}"(?! Margin)', 'Foreground="{StaticResource PrimaryText}"'
    
    if ($changed) {
        Set-Content $file.FullName $content -NoNewline
        Write-Host "  ? Updated $($file.Name)" -ForegroundColor Green
    } else {
        Write-Host "  - No changes needed" -ForegroundColor Gray
    }
}

Write-Host "`n========================================" -ForegroundColor Cyan
Write-Host "  ? FIXES APPLIED!" -ForegroundColor Green
Write-Host "========================================`n" -ForegroundColor Cyan


