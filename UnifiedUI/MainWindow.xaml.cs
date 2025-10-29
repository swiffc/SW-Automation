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

            // PHASE 1: Initialize component tabs based on selected tool
            InitializeComponentTabs();

            // PHASE 1: Subscribe to tool selection changes
            _viewModel.PropertyChanged += ViewModel_PropertyChanged;
        }

        /// <summary>
        /// PHASE 1: Dynamically creates component tabs based on currently selected tool
        /// </summary>
        private void InitializeComponentTabs()
        {
            // Clear existing tabs
            ComponentTabs.Items.Clear();

            // Get components from selected tool (via ViewModel)
            if (_viewModel?.CurrentComponents == null || _viewModel.CurrentComponents.Count == 0)
            {
                // Default to all components if no tool selected
                var componentTypes = new[]
                {
                    "Bundle",
                    "Header",
                    "Hood",
                    "Machinery Mount",
                    "Plenum",
                    "Structure",
                    "Walkway"
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
            else
            {
                // Create tabs for selected tool's components
                foreach (var componentName in _viewModel.CurrentComponents)
                {
                    var tabItem = new TabItem
                    {
                        Header = componentName,
                        Content = CreateComponentPanel(componentName)
                    };
                    ComponentTabs.Items.Add(tabItem);
                }
            }
        }

        /// <summary>
        /// PHASE 1: Handles ViewModel property changes (especially SelectedTool)
        /// </summary>
        private void ViewModel_PropertyChanged(object sender, System.ComponentModel.PropertyChangedEventArgs e)
        {
            if (e.PropertyName == nameof(MainViewModel.SelectedTool))
            {
                // Tool changed - rebuild tabs
                InitializeComponentTabs();
            }
        }

        /// <summary>
        /// PHASE 1: Factory method to create appropriate panel based on component type
        /// IMPROVED: Explicitly sets DataContext for all panels
        /// </summary>
        private UIElement CreateComponentPanel(string componentType)
        {
            UIElement panel = componentType switch
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
                "Certified Drawing" => new TextBlock 
                { 
                    Text = "Hudson Certified Drawing panel - Coming in Phase 2", 
                    Margin = new Thickness(20),
                    FontSize = 14
                },
                _ => new TextBlock 
                { 
                    Text = $"{componentType} panel coming soon...",
                    Margin = new Thickness(20),
                    FontSize = 14
                }
            };

            // CRITICAL FIX: Explicitly set DataContext for all panels
            // This ensures bindings work immediately without relying on Loaded event
            if (panel is FrameworkElement frameworkElement)
            {
                frameworkElement.DataContext = _viewModel;
            }

            return panel;
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
                InitialDirectory = System.IO.Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), "CascadeProjects", "Solidworks_Automation")
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

        /// <summary>
        /// Import Prego button click handler
        /// Imports data from Prego Excel file using Excel.Prego system
        /// See: docs/User_Guide/PREGO_IMPORT_USER_GUIDE.md
        /// </summary>
        private void ImportPregoButton_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                // Call ViewModel's ImportFromPrego method
                // This will auto-locate the Prego file based on Job Number and Bank
                _viewModel.ImportFromPrego();
            }
            catch (Exception ex)
            {
                MessageBox.Show(
                    $"Error importing from Prego:\n\n{ex.Message}\n\n" +
                    $"See troubleshooting guide:\n" +
                    $"docs/User_Guide/PREGO_IMPORT_USER_GUIDE.md",
                    "Import Error",
                    MessageBoxButton.OK,
                    MessageBoxImage.Error);

                FileTools.Infrastructure.GlobalErrorHandler.LogError(ex, "ImportPregoButton_Click");
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
                catch (NotImplementedException)
                {
                    MessageBox.Show(
                        "Excel export functionality is not yet implemented.\n\n" +
                        "This feature is planned for a future release.\n\n" +
                        "Current workaround: Use 'Save' to save configuration internally,\n" +
                        "then manually create Excel file from saved data.",
                        "Feature Not Available",
                        MessageBoxButton.OK,
                        MessageBoxImage.Information);
                }
                catch (Exception ex)
                {
                    MessageBox.Show($"Error exporting to Excel: {ex.Message}", "Error",
                        MessageBoxButton.OK, MessageBoxImage.Error);
                    FileTools.Infrastructure.GlobalErrorHandler.LogError(ex, "ExportButton_Click");
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
                if (progressWindow == null)
                {
                    throw new InvalidOperationException("Failed to create progress window");
                }
                
                progressWindow.Show();

                _viewModel.GenerateSolidWorksComponents((progress) =>
                {
                    Dispatcher.Invoke(() => progressWindow?.UpdateProgress(progress));
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
