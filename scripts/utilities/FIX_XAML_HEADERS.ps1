# Fix XAML Header Attributes - Remove Emojis Properly
# This script fixes the malformed Header="" attributes

Write-Host "`nFixing malformed XAML headers..." -ForegroundColor Yellow

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

foreach ($panelPath in $panels) {
    if (-not (Test-Path $panelPath)) {
        Write-Host "  Skip: $panelPath (not found)" -ForegroundColor Gray
        continue
    }
    
    Write-Host "  Processing: $panelPath" -ForegroundColor Cyan
    $content = Get-Content $panelPath -Raw -Encoding UTF8
    
    # Fix malformed headers with multiple empty quotes
    $content = $content -replace 'Header="{2,}"', 'Header="'
    
    # Fix headers that start with quotes and text
    $content = $content -replace 'Header=""+([A-Z])', 'Header="$1'
    
    # Save
    $content | Set-Content $panelPath -Encoding UTF8 -NoNewline
    Write-Host "    Fixed!" -ForegroundColor Green
}

Write-Host "`n Done! Now rebuild.`n" -ForegroundColor Green

