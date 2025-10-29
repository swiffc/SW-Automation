using System;
using System.Windows.Controls;
using System.Windows;
using UnifiedUI.ViewModels;
using Microsoft.Win32;

namespace UnifiedUI.Views
{
    public partial class BundlePanel : UserControl
    {
        public BundlePanel()
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

        /// <summary>
        /// Import Prego button click - imports data from Prego Excel file
        /// </summary>
        private void BtnImportPrego_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                var viewModel = DataContext as MainViewModel;
                if (viewModel != null)
                {
                    viewModel.ImportFromPrego();
                }
                else
                {
                    MessageBox.Show("ViewModel not available. DataContext is not set.", 
                        "Error", MessageBoxButton.OK, MessageBoxImage.Error);
                }
            }
            catch (Exception ex)
            {
                FileTools.Infrastructure.GlobalErrorHandler.LogError(ex, "BtnImportPrego_Click");
                MessageBox.Show($"Error importing Prego: {ex.Message}", 
                    "Import Error", MessageBoxButton.OK, MessageBoxImage.Error);
            }
        }

        /// <summary>
        /// Save Config button click - saves current configuration
        /// </summary>
        private void BtnSaveConfig_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                var viewModel = DataContext as MainViewModel;
                if (viewModel != null)
                {
                    viewModel.SaveConfiguration();
                    MessageBox.Show("Configuration saved successfully!", 
                        "Success", MessageBoxButton.OK, MessageBoxImage.Information);
                }
                else
                {
                    MessageBox.Show("ViewModel not available. DataContext is not set.", 
                        "Error", MessageBoxButton.OK, MessageBoxImage.Error);
                }
            }
            catch (Exception ex)
            {
                FileTools.Infrastructure.GlobalErrorHandler.LogError(ex, "BtnSaveConfig_Click");
                MessageBox.Show($"Error saving configuration: {ex.Message}", 
                    "Save Error", MessageBoxButton.OK, MessageBoxImage.Error);
            }
        }

        /// <summary>
        /// Export button click - exports configuration to Excel
        /// </summary>
        private void BtnExport_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                var dialog = new SaveFileDialog
                {
                    Filter = "Excel Files (*.xlsx)|*.xlsx|All Files (*.*)|*.*",
                    Title = "Export Bundle Configuration to Excel",
                    DefaultExt = ".xlsx",
                    FileName = $"BundleConfig_{DateTime.Now:yyyyMMdd_HHmmss}.xlsx"
                };

                if (dialog.ShowDialog() == true)
                {
                    var viewModel = DataContext as MainViewModel;
                    if (viewModel != null)
                    {
                        viewModel.ExportToExcel(dialog.FileName);
                        MessageBox.Show($"Configuration exported successfully to:\n{dialog.FileName}", 
                            "Export Success", MessageBoxButton.OK, MessageBoxImage.Information);
                    }
                    else
                    {
                        MessageBox.Show("ViewModel not available. DataContext is not set.", 
                            "Error", MessageBoxButton.OK, MessageBoxImage.Error);
                    }
                }
            }
            catch (NotImplementedException)
            {
                MessageBox.Show(
                    "Excel export functionality is not yet fully implemented.\n\n" +
                    "This feature is planned for a future release.\n\n" +
                    "Current workaround: Use 'Save Config' to save configuration internally.",
                    "Feature Not Available",
                    MessageBoxButton.OK,
                    MessageBoxImage.Information);
            }
            catch (Exception ex)
            {
                FileTools.Infrastructure.GlobalErrorHandler.LogError(ex, "BtnExport_Click");
                MessageBox.Show($"Error exporting: {ex.Message}", 
                    "Export Error", MessageBoxButton.OK, MessageBoxImage.Error);
            }
        }
    }
}
