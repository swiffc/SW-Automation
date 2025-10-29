# Apply Dark Theme to All UnifiedUI Panels
# Quick script to update remaining panel XAML files

$panelsToUpdate = @(
    "HoodPanel.xaml",
    "MachineryMountPanel.xaml",
    "PlenumPanel.xaml",
    "StructurePanel.xaml",
    "WalkwayPanel.xaml",
    "XCHStructurePanel.xaml",
    "ZStructurePanel.xaml"
)

$panelsPath = "macros\csharp\Solidworks-Automation\UnifiedUI\Views"

$resourcesBlock = @"
    
    <UserControl.Resources>
        <!-- Apply dark theme to ALL controls -->
        <Style TargetType="TextBox" BasedOn="{StaticResource ModernTextBox}"/>
        <Style TargetType="ComboBox" BasedOn="{StaticResource ModernComboBox}"/>
        <Style TargetType="Label" BasedOn="{StaticResource ModernLabel}"/>
        <Style TargetType="TextBlock">
            <Setter Property="Foreground" Value="{StaticResource PrimaryText}"/>
        </Style>
        <Style TargetType="Border" x:Key="SectionBorder">
            <Setter Property="Background" Value="{StaticResource SecondaryBackground}"/>
            <Setter Property="BorderBrush" Value="{StaticResource BorderColor}"/>
            <Setter Property="BorderThickness" Value="1"/>
            <Setter Property="CornerRadius" Value="{StaticResource CornerRadiusMedium}"/>
            <Setter Property="Padding" Value="15"/>
            <Setter Property="Effect" Value="{StaticResource SubtleShadow}"/>
        </Style>
    </UserControl.Resources>
    
"@

Write-Host "`n========================================" -ForegroundColor Cyan
Write-Host "  ?? APPLYING DARK THEME TO PANELS" -ForegroundColor Green
Write-Host "========================================`n" -ForegroundColor Cyan

foreach ($panelFile in $panelsToUpdate) {
    $filePath = Join-Path $panelsPath $panelFile
    
    if (Test-Path $filePath) {
        Write-Host "Updating: $panelFile" -ForegroundColor Yellow
        
        $content = Get-Content $filePath -Raw
        
        # Change Background="White" to Background="{StaticResource PrimaryBackground}"
        $content = $content -replace 'Background="White"', 'Background="{StaticResource PrimaryBackground}"'
        
        # Add resources block after UserControl opening tag if not already present
        if ($content -notmatch "<UserControl.Resources>") {
            $content = $content -replace '(Background="\{StaticResource PrimaryBackground\}">)', "`$1`n$resourcesBlock"
        }
        
        # Save updated content
        Set-Content $filePath $content -NoNewline
        
        Write-Host "  ? Updated $panelFile" -ForegroundColor Green
    } else {
        Write-Host "  ? Not found: $panelFile" -ForegroundColor Red
    }
}

Write-Host "`n========================================" -ForegroundColor Cyan
Write-Host "  ? DARK THEME APPLIED TO ALL PANELS!" -ForegroundColor Green
Write-Host "========================================`n" -ForegroundColor Cyan


