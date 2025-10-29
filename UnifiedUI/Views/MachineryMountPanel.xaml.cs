using System.Windows.Controls;
using System.Windows;

namespace UnifiedUI.Views
{
    public partial class MachineryMountPanel : UserControl
    {
        public MachineryMountPanel()
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
