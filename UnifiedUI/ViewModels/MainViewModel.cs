using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;
using UnifiedUI.Models;
using UnifiedUI.Services;

namespace UnifiedUI.ViewModels
{
    /// <summary>
    /// Main ViewModel for the Unified UI
    /// Manages state, validation, and operations for all component types
    /// </summary>
    public class MainViewModel : INotifyPropertyChanged
    {
        private readonly ValidationService _validationService;
        private readonly ExcelService _excelService;
        private readonly SolidWorksService _solidWorksService;
        private readonly TemplateService _templateService;
        private readonly ExcelTemplateImporter _excelTemplateImporter;
        private readonly EngineeringReportGenerator _reportGenerator;

        private string _statusMessage;
        private string _validationStatus;
        private string _validationSummary;
        private string _validationIcon;
        private string _previewDimensions;
        private int _validParameterCount;
        private int _warningCount;
        private int _errorCount;
        
        // Shared properties across all tabs
        private string _globalJobNumber = "S2____";
        private string _globalPartPrefix = "JOBNO-";
        private string _globalRevision = "R01";

        // Bundle-specific properties
        private double _bundleWidth = 48.500;
        private double _sideFrameThickness = 0.375;
        private double _sideFrameDepth = 4.000;

        public MainViewModel()
        {
            _validationService = new ValidationService();
            _excelService = new ExcelService();
            _solidWorksService = new SolidWorksService();
            _templateService = new TemplateService();
            _excelTemplateImporter = new ExcelTemplateImporter();
            _reportGenerator = new EngineeringReportGenerator();

            Templates = new ObservableCollection<Template>();
            ValidationMessages = new ObservableCollection<string>();
            
            // Initialize with a default Bundle configuration
            CurrentConfiguration = new BundleConfiguration
            {
                JobNumber = _globalJobNumber,
                BundleWidth = _bundleWidth,
                SideFrameThickness = _sideFrameThickness,
                SideFrameDepth = _sideFrameDepth
            };
            
            LoadTemplates();
            InitializeValidation();
        }

        #region Properties

        public ObservableCollection<Template> Templates { get; set; }
        public ObservableCollection<string> ValidationMessages { get; set; }

        /// <summary>
        /// Global Job Number - shared across all component tabs
        /// </summary>
        public string GlobalJobNumber
        {
            get => _globalJobNumber;
            set
            {
                if (_globalJobNumber != value)
                {
                    _globalJobNumber = value;
                    OnPropertyChanged();
                    // Update all component configurations
                    UpdateAllComponentJobNumbers(value);
                }
            }
        }

        /// <summary>
        /// Global Part Prefix - shared across all component tabs
        /// </summary>
        public string GlobalPartPrefix
        {
            get => _globalPartPrefix;
            set
            {
                if (_globalPartPrefix != value)
                {
                    _globalPartPrefix = value;
                    OnPropertyChanged();
                }
            }
        }

        /// <summary>
        /// Global Revision - shared across all component tabs
        /// </summary>
        public string GlobalRevision
        {
            get => _globalRevision;
            set
            {
                if (_globalRevision != value)
                {
                    _globalRevision = value;
                    OnPropertyChanged();
                }
            }
        }

        /// <summary>
        /// Bundle Width - updates configuration and dimensions
        /// </summary>
        public double BundleWidth
        {
            get => _bundleWidth;
            set
            {
                if (_bundleWidth != value)
                {
                    _bundleWidth = value;
                    OnPropertyChanged();
                    UpdateBundleConfiguration();
                }
            }
        }

        /// <summary>
        /// Side Frame Thickness
        /// </summary>
        public double SideFrameThickness
        {
            get => _sideFrameThickness;
            set
            {
                if (_sideFrameThickness != value)
                {
                    _sideFrameThickness = value;
                    OnPropertyChanged();
                    UpdateBundleConfiguration();
                }
            }
        }

        /// <summary>
        /// Side Frame Depth
        /// </summary>
        public double SideFrameDepth
        {
            get => _sideFrameDepth;
            set
            {
                if (_sideFrameDepth != value)
                {
                    _sideFrameDepth = value;
                    OnPropertyChanged();
                    UpdateBundleConfiguration();
                }
            }
        }

        public string StatusMessage
        {
            get => _statusMessage;
            set { _statusMessage = value; OnPropertyChanged(); }
        }

        public string ValidationStatus
        {
            get => _validationStatus;
            set { _validationStatus = value; OnPropertyChanged(); }
        }

        public string ValidationSummary
        {
            get => _validationSummary;
            set { _validationSummary = value; OnPropertyChanged(); }
        }

        public string ValidationIcon
        {
            get => _validationIcon;
            set { _validationIcon = value; OnPropertyChanged(); }
        }

        public string PreviewDimensions
        {
            get => _previewDimensions;
            set { _previewDimensions = value; OnPropertyChanged(); }
        }

        public int ValidParameterCount
        {
            get => _validParameterCount;
            set { _validParameterCount = value; OnPropertyChanged(); }
        }

        public int WarningCount
        {
            get => _warningCount;
            set { _warningCount = value; OnPropertyChanged(); }
        }

        public int ErrorCount
        {
            get => _errorCount;
            set { _errorCount = value; OnPropertyChanged(); }
        }

        // Current component configuration
        private ComponentConfiguration _currentConfiguration;
        public ComponentConfiguration CurrentConfiguration
        {
            get => _currentConfiguration;
            set
            {
                if (_currentConfiguration != value)
                {
                    // Unsubscribe from old configuration
                    if (_currentConfiguration != null)
                    {
                        _currentConfiguration.PropertyChanged -= OnConfigurationPropertyChanged;
                    }

                    _currentConfiguration = value;

                    // Subscribe to new configuration
                    if (_currentConfiguration != null)
                    {
                        _currentConfiguration.PropertyChanged += OnConfigurationPropertyChanged;
                    }

                    OnPropertyChanged(nameof(CurrentConfiguration));
                    UpdateDimensionsDisplay();
                    UpdateValidation();
                }
            }
        }

        /// <summary>
        /// Handle property changes in the configuration
        /// </summary>
        private void OnConfigurationPropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            // When any configuration property changes, update dimensions and validation
            UpdateDimensionsDisplay();
            UpdateValidation();
        }

        #endregion

        #region Methods

        public void ClearConfiguration()
        {
            CurrentConfiguration = new ComponentConfiguration();
            StatusMessage = "Configuration cleared";
            UpdateValidation();
        }

        public void SaveConfiguration()
        {
            if (CurrentConfiguration == null)
                throw new InvalidOperationException("No configuration to save");

            var filePath = $"configs/{CurrentConfiguration.ComponentType}_{DateTime.Now:yyyyMMdd_HHmmss}.json";
            _templateService.SaveConfiguration(CurrentConfiguration, filePath);
            
            StatusMessage = $"Configuration saved to {filePath}";
        }

        public void ImportFromExcel(string filePath)
        {
            // Detect component type from Excel file
            var componentType = _excelTemplateImporter.DetectComponentType(filePath);
            
            // Import based on component type
            if (componentType == "Bundle")
            {
                var bundleConfig = _excelTemplateImporter.ImportBundleFromExcel(filePath);
                
                // Set configuration FIRST
                CurrentConfiguration = bundleConfig;
                
                // Then update ALL ViewModel properties to match (triggers UI updates)
                GlobalJobNumber = bundleConfig.JobNumber;
                BundleWidth = bundleConfig.BundleWidth;
                SideFrameThickness = bundleConfig.SideFrameThickness;
                SideFrameDepth = bundleConfig.SideFrameDepth;
                
                // Force refresh all properties
                OnPropertyChanged(nameof(BundleWidth));
                OnPropertyChanged(nameof(SideFrameThickness));
                OnPropertyChanged(nameof(SideFrameDepth));
                OnPropertyChanged(nameof(CurrentConfiguration));
                
                // Update dimensions and validation
                UpdateDimensionsDisplay();
                UpdateValidation();
            }
            else if (componentType == "Header")
            {
                var headerConfig = _excelTemplateImporter.ImportHeaderFromExcel(filePath);
                CurrentConfiguration = headerConfig;
                GlobalJobNumber = headerConfig.JobNumber;
                
                // Force refresh
                OnPropertyChanged(nameof(CurrentConfiguration));
                UpdateDimensionsDisplay();
                UpdateValidation();
            }
            else
            {
                // Generic import for other types
                CurrentConfiguration = _excelService.ImportConfiguration(filePath);
                
                // Force refresh
                OnPropertyChanged(nameof(CurrentConfiguration));
                UpdateDimensionsDisplay();
                UpdateValidation();
            }
            
            StatusMessage = $"✅ Imported {componentType} from {System.IO.Path.GetFileName(filePath)}";
            UpdateValidation();
        }

        public void ExportToExcel(string filePath)
        {
            if (CurrentConfiguration == null)
                throw new InvalidOperationException("No configuration to export");

            _excelService.ExportConfiguration(CurrentConfiguration, filePath);
            StatusMessage = $"Exported to {System.IO.Path.GetFileName(filePath)}";
        }

        public ValidationResult ValidateConfiguration()
        {
            // For now, create a test configuration if none exists
            if (CurrentConfiguration == null)
            {
                CurrentConfiguration = new BundleConfiguration
                {
                    JobNumber = GlobalJobNumber,
                    PartPrefix = GlobalPartPrefix,
                    Revision = GlobalRevision,
                    BundleWidth = 48.5,
                    ComponentType = "Bundle"
                };
            }

            var result = _validationService.Validate(CurrentConfiguration);
            UpdateValidationDisplay(result);
            return result;
        }

        public void GenerateSolidWorksComponents(Action<int> progressCallback)
        {
            // For now, create a test configuration if none exists
            if (CurrentConfiguration == null)
            {
                CurrentConfiguration = new BundleConfiguration
                {
                    JobNumber = GlobalJobNumber,
                    PartPrefix = GlobalPartPrefix,
                    Revision = GlobalRevision,
                    BundleWidth = 48.5,
                    ComponentType = "Bundle"
                };
            }

            var validation = ValidateConfiguration();
            if (!validation.IsValid)
                throw new InvalidOperationException("Configuration is not valid");

            StatusMessage = "Generating SolidWorks components...";
            _solidWorksService.GenerateComponents(CurrentConfiguration, progressCallback);
            StatusMessage = "Generation complete!";
        }

        public void LoadTemplate(Template template)
        {
            if (template == null) return;

            CurrentConfiguration = _templateService.LoadTemplate(template);
            StatusMessage = $"Loaded template: {template.Name}";
            UpdateValidation();
        }

        public void FilterComponents(string searchText)
        {
            // Implement search/filter logic
            StatusMessage = string.IsNullOrWhiteSpace(searchText) 
                ? "Showing all parameters" 
                : $"Filtering by: {searchText}";
        }

        /// <summary>
        /// Generate engineering report for current configuration
        /// </summary>
        public string GenerateReport()
        {
            if (CurrentConfiguration == null)
                throw new InvalidOperationException("No configuration to report");
            
            return _reportGenerator.GenerateTextReport(CurrentConfiguration);
        }
        
        /// <summary>
        /// Save engineering report to file
        /// </summary>
        public void SaveReport(string filePath)
        {
            if (CurrentConfiguration == null)
                throw new InvalidOperationException("No configuration to report");
            
            _reportGenerator.SaveReportToFile(CurrentConfiguration, filePath);
            StatusMessage = $"Report saved to: {System.IO.Path.GetFileName(filePath)}";
        }

        private void LoadTemplates()
        {
            var templates = _templateService.GetAvailableTemplates();
            Templates.Clear();
            foreach (var template in templates)
            {
                Templates.Add(template);
            }
        }

        private void InitializeValidation()
        {
            ValidationStatus = "✅ Ready";
            ValidationSummary = "No configuration loaded";
            ValidationIcon = "○";
            ValidParameterCount = 0;
            WarningCount = 0;
            ErrorCount = 0;
        }

        private void UpdateValidation()
        {
            var result = ValidateConfiguration();
            UpdateValidationDisplay(result);
        }

        private void UpdateValidationDisplay(ValidationResult result)
        {
            ValidationMessages.Clear();

            if (result.IsValid)
            {
                ValidationStatus = "✅ Valid";
                ValidationSummary = "All checks pass - Ready to generate";
                ValidationIcon = "✅";
            }
            else
            {
                ValidationStatus = "❌ Invalid";
                ValidationSummary = $"{result.Errors.Count} error(s) found";
                ValidationIcon = "❌";
            }

            ValidParameterCount = result.ValidCount;
            WarningCount = result.Warnings.Count;
            ErrorCount = result.Errors.Count;

            // Add error messages
            foreach (var error in result.Errors)
            {
                ValidationMessages.Add($"❌ {error}");
            }

            // Add warning messages
            foreach (var warning in result.Warnings)
            {
                ValidationMessages.Add($"⚠️ {warning}");
            }

            // Update preview dimensions
            if (CurrentConfiguration != null)
            {
                PreviewDimensions = $"W: {CurrentConfiguration.Width:F2}\" × " +
                                  $"H: {CurrentConfiguration.Height:F2}\" × " +
                                  $"D: {CurrentConfiguration.Depth:F2}\"";
            }
        }

        /// <summary>
        /// Update job number across all component configurations
        /// </summary>
        private void UpdateAllComponentJobNumbers(string jobNumber)
        {
            // Update current configuration
            if (CurrentConfiguration != null)
            {
                CurrentConfiguration.JobNumber = jobNumber;
            }
            StatusMessage = $"Job Number updated to: {jobNumber}";
        }

        /// <summary>
        /// Update Bundle configuration with current values
        /// </summary>
        private void UpdateBundleConfiguration()
        {
            if (CurrentConfiguration is BundleConfiguration bundleConfig)
            {
                bundleConfig.BundleWidth = _bundleWidth;
                bundleConfig.SideFrameThickness = _sideFrameThickness;
                bundleConfig.SideFrameDepth = _sideFrameDepth;
                bundleConfig.JobNumber = _globalJobNumber;

                // Update dimensions display using synced base properties
                UpdateDimensionsDisplay();
                
                // Trigger validation
                UpdateValidation();
            }
        }

        /// <summary>
        /// Update dimensions display for any configuration type
        /// </summary>
        private void UpdateDimensionsDisplay()
        {
            if (CurrentConfiguration != null)
            {
                PreviewDimensions = $"W: {CurrentConfiguration.Width:F2}\" × " +
                                   $"H: {CurrentConfiguration.Height:F2}\" × " +
                                   $"D: {CurrentConfiguration.Depth:F2}\"";
                OnPropertyChanged(nameof(PreviewDimensions));
            }
        }

        /// <summary>
        /// Generic configuration update - called when any field changes
        /// </summary>
        private void UpdateCurrentConfiguration()
        {
            if (CurrentConfiguration == null) return;

            // Update job info that's common to all
            CurrentConfiguration.JobNumber = _globalJobNumber;
            CurrentConfiguration.PartPrefix = _globalPartPrefix;
            CurrentConfiguration.Revision = _globalRevision;

            // Update dimensions and validation
            UpdateDimensionsDisplay();
            UpdateValidation();
        }

        #endregion

        #region INotifyPropertyChanged

        public event PropertyChangedEventHandler PropertyChanged;

        protected virtual void OnPropertyChanged([CallerMemberName] string propertyName = null)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }

        #endregion
    }
}
