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

        #region Bundle Private Fields - Phase 2

        // Job Information
        private char _bank = 'A';
        private string _customer = "";
        private string _client = "";
        private string _location = "";
        private string _initials = "";
        private string _titleblock = "Hudson";
        private string _purchaseOrder = "";
        private string _itemNumber = "";
        private bool _headersOutsideFrame = false;

        // Tube Configuration
        private double _tubeLength = 96.000;
        private double _tubeProjection = 0.250;
        private double _tubeOD = 1.000;
        private double _tubeWallTHK = 0.035;
        private double _finOD = 1.500;
        private int _tubeRow1Left = 8;
        private int _tubeRow2Left = 7;
        private double _horizontalPitch = 1.500;
        private int _tubeQuantity = 60;
        private double _frontFinStripBack = 0.500;
        private double _rearFinStripBack = 0.500;

        // Tube Supports
        private double _tubeSupportSpacing = 8.000;
        private int _tubeSupportQuantity = 11;
        private string _tubeSupportSize = "C4x5.4";

        // Vertical Pitches - Front
        private double _frontPitch12 = 1.500;
        private double _frontPitch23 = 1.500;
        private double _frontPitch34 = 1.500;
        private double _frontPitch45 = 1.500;
        private double _frontPitch56 = 1.500;
        private double _frontPitch67 = 1.500;
        private double _frontPitch78 = 1.500;
        private double _frontPitch89 = 1.500;
        private double _frontPitch910 = 1.500;

        // Vertical Pitches - Rear
        private double _rearPitch12 = 1.500;
        private double _rearPitch23 = 1.500;
        private double _rearPitch34 = 1.500;
        private double _rearPitch45 = 1.500;
        private double _rearPitch56 = 1.500;
        private double _rearPitch67 = 1.500;
        private double _rearPitch78 = 1.500;
        private double _rearPitch89 = 1.500;
        private double _rearPitch910 = 1.500;

        // Structure & Plenum
        private int _fanCount = 2;
        private double _plenumLength = 120.000;
        private double _offsetFromCenter = 0.000;
        private string _plenumStyle = "Standard";
        private string _columnSize = "W6x20";
        private int _weight = 5000;
        private double _lugStagger = 0.000;
        private double _lugSpacing = 0.000;

        // Advanced Options
        private bool _enableCamber = false;
        private bool _createDrawing = false;
        private bool _saveFiles = true;
        private bool _deleteTemp = false;

        // Calculated Properties
        private int _calculatedTubes = 60;
        private double _calculatedHeight = 36.25;
        private int _calculatedWeight = 5000;

        #endregion

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
        
          // PHASE 3 FIX: Calculate initial totals based on default values
            CalculateTotals();
        
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

        #region Bundle Public Properties - Phase 2

        // Job Information
        public char Bank
        {
   get => _bank;
       set { if (_bank != value) { _bank = value; OnPropertyChanged(); } }
      }

        public string Customer
        {
  get => _customer;
        set { if (_customer != value) { _customer = value; OnPropertyChanged(); } }
        }

     public string Client
        {
            get => _client;
 set { if (_client != value) { _client = value; OnPropertyChanged(); } }
        }

        public string Location
        {
     get => _location;
      set { if (_location != value) { _location = value; OnPropertyChanged(); } }
        }

        public string Initials
        {
            get => _initials;
            set { if (_initials != value) { _initials = value; OnPropertyChanged(); } }
        }

   public string Titleblock
        {
       get => _titleblock;
    set { if (_titleblock != value) { _titleblock = value; OnPropertyChanged(); } }
        }

        public string PurchaseOrder
     {
        get => _purchaseOrder;
         set { if (_purchaseOrder != value) { _purchaseOrder = value; OnPropertyChanged(); } }
    }

        public string ItemNumber
        {
     get => _itemNumber;
 set { if (_itemNumber != value) { _itemNumber = value; OnPropertyChanged(); } }
        }

   public bool HeadersOutsideFrame
   {
            get => _headersOutsideFrame;
            set { if (_headersOutsideFrame != value) { _headersOutsideFrame = value; OnPropertyChanged(); } }
     }

        // Tube Configuration
  public double TubeLength
        {
            get => _tubeLength;
            set { if (Math.Abs(_tubeLength - value) > 0.001) { _tubeLength = value; OnPropertyChanged(); CalculateTotals(); } }
   }

        public double TubeProjection
   {
   get => _tubeProjection;
            set { if (Math.Abs(_tubeProjection - value) > 0.001) { _tubeProjection = value; OnPropertyChanged(); } }
 }

        public double TubeOD
        {
   get => _tubeOD;
 set { if (Math.Abs(_tubeOD - value) > 0.001) { _tubeOD = value; OnPropertyChanged(); CalculateTotals(); } }
        }

        public double TubeWallTHK
   {
            get => _tubeWallTHK;
   set { if (Math.Abs(_tubeWallTHK - value) > 0.001) { _tubeWallTHK = value; OnPropertyChanged(); } }
        }

        public double FinOD
        {
            get => _finOD;
   set { if (Math.Abs(_finOD - value) > 0.001) { _finOD = value; OnPropertyChanged(); } }
        }

        public int TubeRow1Left
     {
         get => _tubeRow1Left;
            set { if (_tubeRow1Left != value) { _tubeRow1Left = value; OnPropertyChanged(); CalculateTotals(); } }
        }

     public int TubeRow2Left
        {
            get => _tubeRow2Left;
          set { if (_tubeRow2Left != value) { _tubeRow2Left = value; OnPropertyChanged(); CalculateTotals(); } }
        }

        public double HorizontalPitch
        {
   get => _horizontalPitch;
  set { if (Math.Abs(_horizontalPitch - value) > 0.001) { _horizontalPitch = value; OnPropertyChanged(); } }
        }

        public int TubeQuantity
        {
  get => _tubeQuantity;
      private set { if (_tubeQuantity != value) { _tubeQuantity = value; OnPropertyChanged(); } }
        }

        public double FrontFinStripBack
        {
            get => _frontFinStripBack;
   set { if (Math.Abs(_frontFinStripBack - value) > 0.001) { _frontFinStripBack = value; OnPropertyChanged(); } }
    }

        public double RearFinStripBack
      {
      get => _rearFinStripBack;
set { if (Math.Abs(_rearFinStripBack - value) > 0.001) { _rearFinStripBack = value; OnPropertyChanged(); } }
     }

// Tube Supports
        public double TubeSupportSpacing
        {
       get => _tubeSupportSpacing;
       set { if (Math.Abs(_tubeSupportSpacing - value) > 0.001) { _tubeSupportSpacing = value; OnPropertyChanged(); } }
        }

        public int TubeSupportQuantity
        {
    get => _tubeSupportQuantity;
  set { if (_tubeSupportQuantity != value) { _tubeSupportQuantity = value; OnPropertyChanged(); } }
        }

        public string TubeSupportSize
     {
            get => _tubeSupportSize;
   set { if (_tubeSupportSize != value) { _tubeSupportSize = value; OnPropertyChanged(); } }
        }

   // Front Vertical Pitches
        public double FrontPitch12
        {
            get => _frontPitch12;
       set { if (Math.Abs(_frontPitch12 - value) > 0.001) { _frontPitch12 = value; OnPropertyChanged(); CalculateTotals(); } }
        }

 public double FrontPitch23
     {
       get => _frontPitch23;
   set { if (Math.Abs(_frontPitch23 - value) > 0.001) { _frontPitch23 = value; OnPropertyChanged(); CalculateTotals(); } }
        }

        public double FrontPitch34
        {
         get => _frontPitch34;
      set { if (Math.Abs(_frontPitch34 - value) > 0.001) { _frontPitch34 = value; OnPropertyChanged(); CalculateTotals(); } }
        }

   public double FrontPitch45
{
            get => _frontPitch45;
    set { if (Math.Abs(_frontPitch45 - value) > 0.001) { _frontPitch45 = value; OnPropertyChanged(); CalculateTotals(); } }
  }

        public double FrontPitch56
        {
 get => _frontPitch56;
            set { if (Math.Abs(_frontPitch56 - value) > 0.001) { _frontPitch56 = value; OnPropertyChanged(); CalculateTotals(); } }
        }

        public double FrontPitch67
 {
            get => _frontPitch67;
            set { if (Math.Abs(_frontPitch67 - value) > 0.001) { _frontPitch67 = value; OnPropertyChanged(); CalculateTotals(); } }
        }

        public double FrontPitch78
      {
          get => _frontPitch78;
            set { if (Math.Abs(_frontPitch78 - value) > 0.001) { _frontPitch78 = value; OnPropertyChanged(); CalculateTotals(); } }
        }

        public double FrontPitch89
        {
            get => _frontPitch89;
            set { if (Math.Abs(_frontPitch89 - value) > 0.001) { _frontPitch89 = value; OnPropertyChanged(); CalculateTotals(); } }
     }

        public double FrontPitch910
        {
            get => _frontPitch910;
    set { if (Math.Abs(_frontPitch910 - value) > 0.001) { _frontPitch910 = value; OnPropertyChanged(); CalculateTotals(); } }
      }

        // Rear Vertical Pitches
        public double RearPitch12
        {
   get => _rearPitch12;
  set { if (Math.Abs(_rearPitch12 - value) > 0.001) { _rearPitch12 = value; OnPropertyChanged(); CalculateTotals(); } }
        }

        public double RearPitch23
        {
   get => _rearPitch23;
     set { if (Math.Abs(_rearPitch23 - value) > 0.001) { _rearPitch23 = value; OnPropertyChanged(); CalculateTotals(); } }
 }

        public double RearPitch34
  {
   get => _rearPitch34;
  set { if (Math.Abs(_rearPitch34 - value) > 0.001) { _rearPitch34 = value; OnPropertyChanged(); CalculateTotals(); } }
        }

        public double RearPitch45
 {
            get => _rearPitch45;
            set { if (Math.Abs(_rearPitch45 - value) > 0.001) { _rearPitch45 = value; OnPropertyChanged(); CalculateTotals(); } }
        }

        public double RearPitch56
        {
            get => _rearPitch56;
       set { if (Math.Abs(_rearPitch56 - value) > 0.001) { _rearPitch56 = value; OnPropertyChanged(); CalculateTotals(); } }
        }

        public double RearPitch67
        {
        get => _rearPitch67;
            set { if (Math.Abs(_rearPitch67 - value) > 0.001) { _rearPitch67 = value; OnPropertyChanged(); CalculateTotals(); } }
        }

        public double RearPitch78
        {
            get => _rearPitch78;
      set { if (Math.Abs(_rearPitch78 - value) > 0.001) { _rearPitch78 = value; OnPropertyChanged(); CalculateTotals(); } }
  }

    public double RearPitch89
    {
     get => _rearPitch89;
            set { if (Math.Abs(_rearPitch89 - value) > 0.001) { _rearPitch89 = value; OnPropertyChanged(); CalculateTotals(); } }
        }

     public double RearPitch910
  {
            get => _rearPitch910;
         set { if (Math.Abs(_rearPitch910 - value) > 0.001) { _rearPitch910 = value; OnPropertyChanged(); CalculateTotals(); } }
      }

   // Structure & Plenum
        public int FanCount
        {
            get => _fanCount;
  set { if (_fanCount != value) { _fanCount = value; OnPropertyChanged(); } }
        }

        public double PlenumLength
        {
          get => _plenumLength;
 set { if (Math.Abs(_plenumLength - value) > 0.001) { _plenumLength = value; OnPropertyChanged(); } }
     }

        public double OffsetFromCenter
   {
         get => _offsetFromCenter;
            set { if (Math.Abs(_offsetFromCenter - value) > 0.001) { _offsetFromCenter = value; OnPropertyChanged(); } }
        }

   public string PlenumStyle
        {
       get => _plenumStyle;
            set { if (_plenumStyle != value) { _plenumStyle = value; OnPropertyChanged(); } }
        }

        public string ColumnSize
{
    get => _columnSize;
            set { if (_columnSize != value) { _columnSize = value; OnPropertyChanged(); } }
     }

        public int Weight
        {
      get => _weight;
     set { if (_weight != value) { _weight = value; OnPropertyChanged(); CalculateTotals(); } }
        }

        public double LugStagger
        {
            get => _lugStagger;
            set { if (Math.Abs(_lugStagger - value) > 0.001) { _lugStagger = value; OnPropertyChanged(); } }
        }

        public double LugSpacing
        {
         get => _lugSpacing;
            set { if (Math.Abs(_lugSpacing - value) > 0.001) { _lugSpacing = value; OnPropertyChanged(); } }
      }

      // Advanced Options
      public bool EnableCamber
        {
            get => _enableCamber;
            set { if (_enableCamber != value) { _enableCamber = value; OnPropertyChanged(); } }
        }

        public bool CreateDrawing
        {
  get => _createDrawing;
            set { if (_createDrawing != value) { _createDrawing = value; OnPropertyChanged(); } }
    }

        public bool SaveFiles
      {
            get => _saveFiles;
   set { if (_saveFiles != value) { _saveFiles = value; OnPropertyChanged(); } }
        }

        public bool DeleteTemp
        {
       get => _deleteTemp;
     set { if (_deleteTemp != value) { _deleteTemp = value; OnPropertyChanged(); } }
        }

  // Calculated Properties (Read-only)
        public int CalculatedTubes
   {
            get => _calculatedTubes;
   private set { if (_calculatedTubes != value) { _calculatedTubes = value; OnPropertyChanged(); } }
        }

        public double CalculatedHeight
        {
            get => _calculatedHeight;
      private set { if (Math.Abs(_calculatedHeight - value) > 0.001) { _calculatedHeight = value; OnPropertyChanged(); } }
    }

        public int CalculatedWeight
        {
     get => _calculatedWeight;
            private set { if (_calculatedWeight != value) { _calculatedWeight = value; OnPropertyChanged(); } }
        }

    #endregion

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

            var filePath = $"configs/{CurrentConfiguration.ComponentType}_{DateTime.Now:yyyyMMdd_HHmms}.json";
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

  /// <summary>
        /// Auto-calculate totals when relevant fields change - PHASE 2
        /// </summary>
      private void CalculateTotals()
        {
  // Calculate total tubes (both sides)
   CalculatedTubes = (TubeRow1Left + TubeRow2Left) * 2;

   // Calculate bundle height from vertical pitches
            double totalFrontPitches = FrontPitch12 + FrontPitch23 + FrontPitch34 + FrontPitch45 + 
    FrontPitch56 + FrontPitch67 + FrontPitch78 + FrontPitch89 + FrontPitch910;
    double totalRearPitches = RearPitch12 + RearPitch23 + RearPitch34 + RearPitch45 + 
 RearPitch56 + RearPitch67 + RearPitch78 + RearPitch89 + RearPitch910;
   
            // Average height + tube OD + margins
      double avgPitches = (totalFrontPitches + totalRearPitches) / 2.0;
       CalculatedHeight = avgPitches + (TubeOD * 2) + 2.0; // Add margins

 // Calculate estimated weight
   double tubeWeight = CalculatedTubes * TubeLength * 0.12; // Approximate lb/ft
   double frameWeight = BundleWidth * SideFrameDepth * 2 * 0.5;
            CalculatedWeight = (int)(tubeWeight + frameWeight + Weight);
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
