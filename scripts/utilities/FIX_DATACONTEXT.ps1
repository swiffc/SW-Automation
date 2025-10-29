# Fix all UserControl DataContext inheritance issues
$panels = @(
    "macros\csharp\Solidworks-Automation\UnifiedUI\Views\HoodPanel.xaml.cs",
    "macros\csharp\Solidworks-Automation\UnifiedUI\Views\MachineryMountPanel.xaml.cs",
    "macros\csharp\Solidworks-Automation\UnifiedUI\Views\PlenumPanel.xaml.cs",
    "macros\csharp\Solidworks-Automation\UnifiedUI\Views\StructurePanel.xaml.cs",
    "macros\csharp\Solidworks-Automation\UnifiedUI\Views\WalkwayPanel.xaml.cs",
    "macros\csharp\Solidworks-Automation\UnifiedUI\Views\XCHStructurePanel.xaml.cs",
    "macros\csharp\Solidworks-Automation\UnifiedUI\Views\ZStructurePanel.xaml.cs"
)

foreach ($panel in $panels) {
    $panelName = [System.IO.Path]::GetFileNameWithoutExtension($panel).Replace(".xaml", "")
    
    Write-Host "Fixing DataContext for $panelName..." -ForegroundColor Yellow
    
    $content = @"
using System.Windows.Controls;
using System.Windows;

namespace UnifiedUI.Views
{
    public partial class $panelName : UserControl
    {
        public $panelName()
        {
            InitializeComponent();
            
            // Ensure DataContext is inherited from parent Window
            this.Loaded += (sender, e) =>
            {
                var window = Window.GetWindow(this);
                if (window != null && this.DataContext == null)
                {
                    this.DataContext = window.DataContext;
                }
            };
        }
    }
}
"@
    
    $content | Out-File -FilePath $panel -Encoding UTF8
    Write-Host "Fixed $panelName" -ForegroundColor Green
}

Write-Host "`nAll UserControl DataContext issues fixed!" -ForegroundColor Cyan