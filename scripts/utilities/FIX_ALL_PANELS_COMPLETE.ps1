# Fix ALL Panel Issues - Complete Script
# Removes emojis, fixes backgrounds, applies dark theme

Write-Host "`n========================================" -ForegroundColor Cyan
Write-Host "  PHASE 1: COMPLETE PANEL FIXES" -ForegroundColor Green
Write-Host "========================================`n" -ForegroundColor Cyan

$panels = @(
    "macros\csharp\Solidworks-Automation\UnifiedUI\Views\BundlePanel.xaml",
    "macros\csharp\Solidworks-Automation\UnifiedUI\Views\HeaderSimplePanel.xaml",
    "macros\csharp\Solidworks-Automation\UnifiedUI\Views\HoodPanel.xaml",
    "macros\csharp\Solidworks-Automation\UnifiedUI\Views\MachineryMountPanel.xaml",
    "macros\csharp\Solidworks-Automation\UnifiedUI\Views\PlenumPanel.xaml",
    "macros\csharp\Solidworks-Automation\UnifiedUI\Views\StructurePanel.xaml",
    "macros\csharp\Solidworks-Automation\UnifiedUI\Views\WalkwayPanel.xaml",
    "macros\csharp\Solidworks-Automation\UnifiedUI\Views\XCHStructurePanel.xaml",
    "macros\csharp\Solidworks-Automation\UnifiedUI\Views\ZStructurePanel.xaml"
)

$fixCount = 0
$fileCount = 0

foreach ($panelPath in $panels) {
    if (-not (Test-Path $panelPath)) {
        Write-Host "??  File not found: $panelPath" -ForegroundColor Yellow
        continue
    }
    
    Write-Host "Processing: $panelPath" -ForegroundColor Cyan
    $content = Get-Content $panelPath -Raw -Encoding UTF8
    $original = $content
    
    # === EMOJI REMOVAL ===
    # Remove ALL emoji patterns
    $content = $content -replace 'Header="??\s*', 'Header="'
    $content = $content -replace 'Header="??\s*', 'Header="'
    $content = $content -replace 'Header="??\s*', 'Header="'
    $content = $content -replace 'Header="??\s*', 'Header="'
    $content = $content -replace 'Header="??\s*', 'Header="'
    $content = $content -replace 'Header="??\s*', 'Header="'
    $content = $content -replace 'Header="??\s*', 'Header="'
    $content = $content -replace 'Header="???\s*', 'Header="'
    
    # Remove ?? encoded emojis
    $content = $content -replace 'Header="\?\?\s*', 'Header="'
    $content = $content -replace 'Header="ðŸ[^"]*\s*', 'Header="'  # Malformed UTF-8
    
    # === BACKGROUND FIXES ===
    # Fix hardcoded white/light backgrounds
    $content = $content -replace 'Background="#FFFFFF"', 'Background="{StaticResource PrimaryBackground}"'
    $content = $content -replace 'Background="White"', 'Background="{StaticResource PrimaryBackground}"'
    $content = $content -replace 'Background="#F5F5F5"', 'Background="{StaticResource SecondaryBackground}"'
    $content = $content -replace 'Background="#EEEEEE"', 'Background="{StaticResource SecondaryBackground}"'
    $content = $content -replace 'Background="LightGray"', 'Background="{StaticResource SecondaryBackground}"'
    
    # Fix expander borders (remove explicit colored borders)
    $content = $content -replace 'BorderBrush="#4CAF50"', 'BorderBrush="{StaticResource SuccessColor}"'
    $content = $content -replace 'BorderBrush="#FF9800"', 'BorderBrush="{StaticResource WarningColor}"'
    $content = $content -replace 'BorderBrush="#2196F3"', 'BorderBrush="{StaticResource PrimaryAccent}"'
    $content = $content -replace 'BorderBrush="#F44336"', 'BorderBrush="{StaticResource ErrorColor}"'
    
    # === FIX TEXTBLOCK FOREGROUNDS ===
    $content = $content -replace 'Foreground="#666"', 'Foreground="{StaticResource SecondaryText}"'
    $content = $content -replace 'Foreground="#333"', 'Foreground="{StaticResource PrimaryText}"'
    $content = $content -replace 'Foreground="Black"', 'Foreground="{StaticResource PrimaryText}"'
    
    if ($content -ne $original) {
        $content | Set-Content $panelPath -Encoding UTF8 -NoNewline
        Write-Host "  ? Fixed!" -ForegroundColor Green
        $fixCount++
    } else {
        Write-Host "  ??  No changes needed" -ForegroundColor Gray
    }
    
    $fileCount++
}

Write-Host "`n========================================" -ForegroundColor Cyan
Write-Host "  PHASE 1 COMPLETE!" -ForegroundColor Green
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Files processed: $fileCount" -ForegroundColor White
Write-Host "Files updated: $fixCount" -ForegroundColor White
Write-Host "`nNext: Rebuild UnifiedUI" -ForegroundColor Yellow


