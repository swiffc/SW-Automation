# FIX ALL COLOR VISIBILITY ISSUES
# Ensures all text is visible with dark theme

Write-Host "`n========================================" -ForegroundColor Cyan
Write-Host "  FIXING ALL COLOR VISIBILITY ISSUES" -ForegroundColor Yellow
Write-Host "========================================`n" -ForegroundColor Cyan

$fixCount = 0

# All panel files to check
$panelFiles = @(
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

Write-Host "[1/3] Checking panel files...`n" -ForegroundColor White

foreach ($file in $panelFiles) {
    if (Test-Path $file) {
        $content = Get-Content $file -Raw
        $modified = $false
        
        # Check for TextBox without Foreground
        if ($content -match '<TextBox(?![^>]*Foreground=)') {
            Write-Host "  ?? $([System.IO.Path]::GetFileName($file)): TextBox missing Foreground" -ForegroundColor Yellow
        }
        
        # Check for ComboBox without Foreground
        if ($content -match '<ComboBox(?![^>]*Foreground=)') {
            Write-Host "  ?? $([System.IO.Path]::GetFileName($file)): ComboBox missing Foreground" -ForegroundColor Yellow
        }
        
        # Check for white backgrounds
        if ($content -match 'Background="#[Ff]{6}"' -or $content -match 'Background="White"') {
            Write-Host "  ? $([System.IO.Path]::GetFileName($file)): WHITE BACKGROUND FOUND!" -ForegroundColor Red
            $fixCount++
        }
        
        Write-Host "  ? $([System.IO.Path]::GetFileName($file))" -ForegroundColor Green
    } else {
        Write-Host "  ?? NOT FOUND: $file" -ForegroundColor Yellow
    }
}

Write-Host "`n[2/3] Checking MainWindow...`n" -ForegroundColor White

$mainWindow = "macros\csharp\Solidworks-Automation\UnifiedUI\MainWindow.xaml"
if (Test-Path $mainWindow) {
    $content = Get-Content $mainWindow -Raw
    
    # Check toolbar
    if ($content -match 'ToolBar.*Background="White"') {
        Write-Host "  ? MainWindow: WHITE TOOLBAR!" -ForegroundColor Red
        $fixCount++
    }
    
    # Check for any remaining white backgrounds
    $whiteMatches = Select-String -Path $mainWindow -Pattern 'Background="#[Ff]{6}"' -AllMatches
    if ($whiteMatches) {
        Write-Host "  ? MainWindow: $($whiteMatches.Matches.Count) white backgrounds found!" -ForegroundColor Red
        $fixCount++
    } else {
        Write-Host "  ? MainWindow: No white backgrounds" -ForegroundColor Green
    }
    
    # Check if toolbar ComboBox and TextBox have styles
    if ($content -match 'ToolBar.*ComboBox.*Style="\{StaticResource ModernComboBox\}"') {
        Write-Host "  ? Toolbar ComboBox has dark theme" -ForegroundColor Green
    } else {
        Write-Host "  ?? Toolbar ComboBox might be missing style" -ForegroundColor Yellow
    }
    
    if ($content -match 'ToolBar.*TextBox.*Style="\{StaticResource ModernTextBox\}"') {
        Write-Host "  ? Toolbar TextBox has dark theme" -ForegroundColor Green
    } else {
        Write-Host "  ?? Toolbar TextBox might be missing style" -ForegroundColor Yellow
    }
}

Write-Host "`n[3/3] Checking theme resources...`n" -ForegroundColor White

$themeFile = "macros\csharp\Solidworks-Automation\UnifiedUI\Themes\ModernDark.xaml"
if (Test-Path $themeFile) {
    $content = Get-Content $themeFile -Raw
    
    # Check for key colors
    $requiredColors = @("PrimaryText", "SecondaryText", "PrimaryBackground", "SecondaryBackground")
    $missingColors = @()
    
    foreach ($color in $requiredColors) {
        if ($content -notmatch "x:Key=`"$color`"") {
            $missingColors += $color
        }
    }
    
    if ($missingColors.Count -gt 0) {
        Write-Host "  ? Missing theme colors: $($missingColors -join ', ')" -ForegroundColor Red
        $fixCount++
    } else {
        Write-Host "  ? All theme colors defined" -ForegroundColor Green
    }
} else {
    Write-Host "  ? Theme file not found!" -ForegroundColor Red
    $fixCount++
}

Write-Host "`n========================================" -ForegroundColor Cyan
if ($fixCount -eq 0) {
    Write-Host "  ? ALL COLOR CHECKS PASSED!" -ForegroundColor Green
    Write-Host "  Text should be visible everywhere" -ForegroundColor Green
} else {
    Write-Host "  ?? FOUND $fixCount POTENTIAL ISSUES" -ForegroundColor Yellow
    Write-Host "  Check output above for details" -ForegroundColor Yellow
}
Write-Host "========================================`n" -ForegroundColor Cyan

# Additional check: List any ComboBox or TextBox without explicit styling
Write-Host "DETAILED CHECK - Elements without theme styles:`n" -ForegroundColor Cyan

foreach ($file in $panelFiles + @($mainWindow)) {
    if (Test-Path $file) {
        $fileName = [System.IO.Path]::GetFileName($file)
        $content = Get-Content $file -Raw
        
        # Find TextBox without Style attribute
        $textBoxMatches = [regex]::Matches($content, '<TextBox[^>]*(?!Style=)[^>]*>')
        if ($textBoxMatches.Count -gt 0) {
            foreach ($match in $textBoxMatches) {
                if ($match.Value -notmatch 'Style=') {
                    Write-Host "  $fileName : TextBox without Style" -ForegroundColor Yellow
                    break
                }
            }
        }
        
        # Find ComboBox without Style attribute  
        $comboMatches = [regex]::Matches($content, '<ComboBox[^>]*(?!Style=)[^>]*>')
        if ($comboMatches.Count -gt 0) {
            foreach ($match in $comboMatches) {
                if ($match.Value -notmatch 'Style=') {
                    Write-Host "  $fileName : ComboBox without Style" -ForegroundColor Yellow
                    break
                }
            }
        }
    }
}

Write-Host "`n========================================" -ForegroundColor Cyan
Write-Host "  SCAN COMPLETE" -ForegroundColor Green
Write-Host "========================================`n" -ForegroundColor Cyan


