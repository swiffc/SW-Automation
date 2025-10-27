using System;
using System.Windows;
using System.Windows.Controls;
using UnifiedUI.ViewModels;
using UnifiedUI.Models;

namespace UnifiedUI
{
    /// <summary>
    /// Main Window for Unified SolidWorks Automation UI
    /// Replaces 7 separate table-based UIs with single modern interface
    /// </summary>
    public partial class MainWindow : Window
    {
        private readonly MainViewModel _viewModel;

        public MainWindow()
        {
            InitializeComponent();
            _viewModel = new MainViewModel();
            DataContext = _viewModel;

            // Initialize component tabs
            InitializeComponentTabs();
        }

        private void InitializeComponentTabs()
        {
            // Create tab items for each component type
            var componentTypes = new[]
            {
                "Bundle",
                "Header",
                "Hood",
                "Machinery Mount",
                "Plenum",
                "Structure",
                "Walkway",
                "XCH Structure",
                "Z Structure"
            };

            foreach (var componentType in componentTypes)
            {
                var tabItem = new TabItem
                {
                    Header = componentType,
                    Content = CreateComponentPanel(componentType)
                };
                ComponentTabs.Items.Add(tabItem);
            }
        }

        private UIElement CreateComponentPanel(string componentType)
        {
            // Factory method to create appropriate panel based on component type
            return componentType switch
            {
                "Bundle" => new Views.BundlePanel(),
                "Header" => new Views.HeaderSimplePanel(),
                "Hood" => new Views.HoodPanel(),
                "Machinery Mount" => new Views.MachineryMountPanel(),
                "Plenum" => new Views.PlenumPanel(),
                "Structure" => new Views.StructurePanel(),
                "Walkway" => new Views.WalkwayPanel(),
                "XCH Structure" => new Views.XCHStructurePanel(),
                "Z Structure" => new Views.ZStructurePanel(),
                _ => new TextBlock { Text = $"{componentType} panel coming soon..." }
            };
        }

        #region Event Handlers

        private void NewButton_Click(object sender, RoutedEventArgs e)
        {
            if (MessageBox.Show("Clear current configuration and start new?", 
                "New Configuration", MessageBoxButton.YesNo, MessageBoxImage.Question) == MessageBoxResult.Yes)
            {
                _viewModel.ClearConfiguration();
            }
        }

        private void SaveButton_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                _viewModel.SaveConfiguration();
                MessageBox.Show("Configuration saved successfully!", "Success", 
                    MessageBoxButton.OK, MessageBoxImage.Information);
            }
            catch (Exception ex)
            {
                MessageBox.Show($"Error saving configuration: {ex.Message}", "Error", 
                    MessageBoxButton.OK, MessageBoxImage.Error);
            }
        }

        private void ImportButton_Click(object sender, RoutedEventArgs e)
        {
            var dialog = new Microsoft.Win32.OpenFileDialog
            {
                Filter = "Excel Template Files (*.xlsm;*.xlsx;*.xls)|*.xlsm;*.xlsx;*.xls|All Files (*.*)|*.*",
                Title = "Import Excel Template (Optional)",
                InitialDirectory = @"c:\Users\DCornealius\CascadeProjects\Solidworks_Automation"
            };

            if (dialog.ShowDialog() == true)
            {
                try
                {
                    _viewModel.ImportFromExcel(dialog.FileName);
                    MessageBox.Show("Excel file imported successfully!", "Success",
                        MessageBoxButton.OK, MessageBoxImage.Information);
                }
                catch (Exception ex)
                {
                    MessageBox.Show($"Error importing Excel: {ex.Message}", "Error",
                        MessageBoxButton.OK, MessageBoxImage.Error);
                }
            }
        }

        private void ExportButton_Click(object sender, RoutedEventArgs e)
        {
            var dialog = new Microsoft.Win32.SaveFileDialog
            {
                Filter = "Excel Files (*.xlsx)|*.xlsx|All Files (*.*)|*.*",
                Title = "Export to Excel",
                DefaultExt = ".xlsx"
            };

            if (dialog.ShowDialog() == true)
            {
                try
                {
                    _viewModel.ExportToExcel(dialog.FileName);
                    MessageBox.Show("Configuration exported to Excel successfully!", "Success",
                        MessageBoxButton.OK, MessageBoxImage.Information);
                }
                catch (Exception ex)
                {
                    MessageBox.Show($"Error exporting to Excel: {ex.Message}", "Error",
                        MessageBoxButton.OK, MessageBoxImage.Error);
                }
            }
        }

        private void GenerateButton_Click(object sender, RoutedEventArgs e)
        {
            // Validate before generating
            var validationResult = _viewModel.ValidateConfiguration();
            if (!validationResult.IsValid)
            {
                MessageBox.Show(
                    $"Configuration has errors:\n\n{string.Join("\n", validationResult.Errors)}", 
                    "Validation Failed", 
                    MessageBoxButton.OK, 
                    MessageBoxImage.Warning);
                return;
            }

            // Show progress dialog and generate
            try
            {
                var progressWindow = new Views.ProgressWindow();
                progressWindow.Show();

                _viewModel.GenerateSolidWorksComponents((progress) =>
                {
                    Dispatcher.Invoke(() => progressWindow.UpdateProgress(progress));
                });

                progressWindow.Close();
                MessageBox.Show("SolidWorks components generated successfully!", "Success",
                    MessageBoxButton.OK, MessageBoxImage.Information);
            }
            catch (Exception ex)
            {
                MessageBox.Show($"Error generating components: {ex.Message}", "Error",
                    MessageBoxButton.OK, MessageBoxImage.Error);
            }
        }

        private void TemplateComboBox_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            if (TemplateComboBox.SelectedItem is Template template)
            {
                _viewModel.LoadTemplate(template);
            }
        }

        private void SearchBox_TextChanged(object sender, TextChangedEventArgs e)
        {
            var searchText = SearchBox.Text;
            _viewModel.FilterComponents(searchText);
        }

        #endregion
    }
}
