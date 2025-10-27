using System.Windows;

namespace UnifiedUI.Views
{
    public partial class ProgressWindow : Window
    {
        public ProgressWindow()
        {
            InitializeComponent();
        }

        public void UpdateProgress(int percent)
        {
            ProgressBar.Value = percent;
            PercentText.Text = $"{percent}%";
            
            if (percent < 25)
                StatusText.Text = "Validating configuration...";
            else if (percent < 50)
                StatusText.Text = "Preparing components...";
            else if (percent < 75)
                StatusText.Text = "Generating SolidWorks files...";
            else if (percent < 100)
                StatusText.Text = "Finalizing...";
            else
                StatusText.Text = "Complete!";
        }
    }
}
