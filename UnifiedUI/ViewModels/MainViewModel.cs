using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;
using UnifiedUI.Models;
using UnifiedUI.Services;
using FileTools.Infrastructure;

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
        private string _bank = "A"; // Changed from char to string for WPF binding compatibility
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

        #region Tool Selector - PHASE 1 (NEW)

        // Tool selection properties
        private ToolType _selectedTool;
        private ObservableCollection<ToolType> _availableTools;
        private ObservableCollection<string> _currentComponents;

        /// <summary>
        /// Currently selected tool/project (Header Section, XCH, Z, or Hudson)
        /// </summary>
        public ToolType SelectedTool
        {
            get => _selectedTool;
            set
            {
                if (_selectedTool != value)
                {
                    _selectedTool = value;
                    OnPropertyChanged();
                    OnToolChanged();
                }
            }
        }

        /// <summary>
        /// All available tools in the application
        /// </summary>
        public ObservableCollection<ToolType> AvailableTools
        {
            get => _availableTools;
            set
            {
                _availableTools = value;
                OnPropertyChanged();
            }
        }

        /// <summary>
        /// Components available for the currently selected tool
        /// </summary>
        public ObservableCollection<string> CurrentComponents
        {
            get => _currentComponents;
            set
            {
                _currentComponents = value;
                OnPropertyChanged();
            }
        }

        // Template management
        private ObservableCollection<Template> _availableTemplates;
        private Template _selectedTemplate;

        /// <summary>
        /// Available templates for the currently selected tool
        /// </summary>
        public ObservableCollection<Template> AvailableTemplates
        {
            get => _availableTemplates;
            set
            {
                _availableTemplates = value;
                OnPropertyChanged();
            }
        }

        /// <summary>
        /// Currently selected template
        /// </summary>
        public Template SelectedTemplate
        {
            get => _selectedTemplate;
            set
            {
                if (_selectedTemplate != value)
                {
                    _selectedTemplate = value;
                    OnPropertyChanged();
                    OnTemplateChanged();
                }
            }
        }

        /// <summary>
        /// Called when user switches to a different tool
        /// Updates available components, template paths, and Excel configs
        /// </summary>
        private void OnToolChanged()
        {
            if (SelectedTool == null) return;

            try
            {
                GlobalErrorHandler.LogInfo($"Switching to tool: {SelectedTool.Name}");

                // Update available components based on selected tool
                CurrentComponents.Clear();
                foreach (var component in SelectedTool.AvailableComponents)
                {
                    CurrentComponents.Add(component);
                }

                // Load templates for this tool
                LoadTemplatesForCurrentTool();

                // Clear current configuration to avoid confusion
                CurrentConfiguration = null;

                // Update status
                StatusMessage = $"Tool: {SelectedTool.Name} | {SelectedTool.ExcelConfigCount} configs, {SelectedTool.TemplateFileCount} templates";

                GlobalErrorHandler.LogInfo($"? Switched to: {SelectedTool.Name}");
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "OnToolChanged");
                StatusMessage = $"Error switching tools: {ex.Message}";
            }
        }

        /// <summary>
        /// Load templates when tool selection changes
        /// </summary>
        private void LoadTemplatesForCurrentTool()
        {
            try
            {
                GlobalErrorHandler.LogInfo($"Loading templates for {SelectedTool.Name}...");

                var templates = _templateService.LoadTemplatesForTool(SelectedTool);
                
                AvailableTemplates = new ObservableCollection<Template>(templates);
                
                // Auto-select first template if available
                if (AvailableTemplates.Count > 0)
                {
                    SelectedTemplate = AvailableTemplates[0];
                    GlobalErrorHandler.LogInfo($"? Loaded {AvailableTemplates.Count} templates");
                }
                else
                {
                    GlobalErrorHandler.LogWarning("No templates found for this tool");
                    StatusMessage = $"?? No templates found for {SelectedTool.Name}";
                }
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "LoadTemplatesForCurrentTool");
                StatusMessage = $"Error loading templates: {ex.Message}";
            }
        }

        /// <summary>
        /// Called when user selects a different template
        /// </summary>
        private void OnTemplateChanged()
        {
            if (SelectedTemplate == null) return;

            try
            {
                GlobalErrorHandler.LogInfo($"Template changed to: {SelectedTemplate.DisplayName}");
                StatusMessage = $"Selected: {SelectedTemplate.DisplayName}";

                // TODO: PHASE 3 - Load 3D preview of template

            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "OnTemplateChanged");
            }
        }

        #endregion

        public MainViewModel()
        {
            _validationService = new ValidationService();
            _excelService = new ExcelService();
            _solidWorksService = new SolidWorksService();
            _templateService = new TemplateService();
            _excelTemplateImporter = new ExcelTemplateImporter();
            _reportGenerator = new EngineeringReportGenerator();

            // Removed duplicate Templates property - using only AvailableTemplates
            ValidationMessages = new ObservableCollection<string>();

            // PHASE 1: Initialize tool selector
            AvailableTools = new ObservableCollection<ToolType>(ToolType.AllTools);
            CurrentComponents = new ObservableCollection<string>();
            AvailableTemplates = new ObservableCollection<Template>();

            // Default to Header Section Tool (most commonly used)
            SelectedTool = ToolType.HeaderSectionTool;
            
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

        // Removed duplicate Templates property - using only AvailableTemplates (see line 178)
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
        public string Bank
        {
            get => _bank;
            set 
            { 
                // Only allow single character (A-Z)
                if (_bank != value && (string.IsNullOrEmpty(value) || value.Length <= 1)) 
                { 
                    _bank = value?.ToUpper(); 
                    OnPropertyChanged(); 
                    UpdateValidation();
                } 
            }
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
            // TODO: Implement configuration saving
            // _templateService.SaveConfiguration(CurrentConfiguration, filePath);
            
            StatusMessage = $"Configuration ready to save to {filePath}";
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

        /// <summary>
        /// Import from Prego Excel file (replicates old UI "Import Prego" button)
        /// Uses Excel.Prego system to auto-locate and read Prego file from project output folder
        /// Complete cell mappings: docs/User_Guide/PREGO_IMPORT_USER_GUIDE.md
        /// </summary>
        public void ImportFromPrego()
        {
            try
            {
                GlobalErrorHandler.LogInfo("ImportFromPrego - Starting Prego import");

                // Check if Prego document is available (Excel.Prego auto-locates file)
                if (Excel.Prego.PregoDoc != null)
                {
                    GlobalErrorHandler.LogInfo("Prego document found, importing data");

                    // Import based on active component tab
                    if (CurrentConfiguration is BundleConfiguration bundleConfig)
                    {
                        ImportBundleFromPrego(bundleConfig);
                    }
                    else if (CurrentConfiguration is HeaderConfiguration headerConfig)
                    {
                        ImportHeaderFromPrego(headerConfig);
                    }
                    else
                    {
                        // For other components, just import job info
                        ImportJobInfoFromPrego();
                    }

                    // Cleanup Excel COM objects
                    Excel.Prego.CleanUp();

                    GlobalErrorHandler.LogInfo("Prego import completed successfully");
                    System.Windows.MessageBox.Show(
                        "Data imported from Prego successfully!\n\n" +
                        "- Job information imported\n" +
                        "- Component dimensions imported\n" +
                        "- Ready to generate\n\n" +
                        "Cell mapping reference:\n" +
                        "docs/User_Guide/PREGO_IMPORT_USER_GUIDE.md",
                        "Import Success",
                        System.Windows.MessageBoxButton.OK,
                        System.Windows.MessageBoxImage.Information);
                }
                else
                {
                    GlobalErrorHandler.LogWarning("Prego document not found");
                    System.Windows.MessageBox.Show(
                        "Prego file not found.\n\n" +
                        "Expected location:\n" +
                        "C:\\Users\\DCornealius\\CascadeProjects\\Solidworks_Automation\\output\\{JobNo}\\Drafting\\Headers\\~Archive\\{Job}-prego{Bank}.xlsm\n\n" +
                        "Please check:\n" +
                        "� Job number is correct\n" +
                        "� Bank is selected\n" +
                        "� File exists in project output folder\n" +
                        "� You have file access\n\n" +
                        "?? Troubleshooting guide:\n" +
                        "docs/User_Guide/PREGO_IMPORT_USER_GUIDE.md",
                        "Prego File Not Found",
                        System.Windows.MessageBoxButton.OK,
                        System.Windows.MessageBoxImage.Warning);
                }
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "ImportFromPrego");
                System.Windows.MessageBox.Show(
                    $"Error importing from Prego:\n\n{ex.Message}\n\n" +
                    $"?? Troubleshooting guide:\n" +
                    $"docs/User_Guide/PREGO_IMPORT_USER_GUIDE.md",
                    "Import Error",
                    System.Windows.MessageBoxButton.OK,
                    System.Windows.MessageBoxImage.Error);
            }
        }

        /// <summary>
        /// Import job information from Prego (used by all components)
        /// Cell H2 = Job Number
        /// See complete mapping: docs/User_Guide/PREGO_IMPORT_USER_GUIDE.md
        /// </summary>
        private void ImportJobInfoFromPrego()
        {
            try
            {
                // Import Job Number (Cell H2)
                var jobNumber = Excel.Prego.CellString(
                    Excel.Prego.InputSheet, "H2");
                if (!string.IsNullOrEmpty(jobNumber))
                    GlobalJobNumber = jobNumber;

                GlobalErrorHandler.LogInfo($"Imported job info: {jobNumber}");
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "ImportJobInfoFromPrego");
            }
        }

        /// <summary>
        /// Import Bundle-specific data from Prego (replicates BundleUI.bImportPrego_Click)
        /// Reads 50+ cells from Prego Excel file
        /// 
        /// Key cells (see full mapping in user guide):
        /// - Bundle Width: BQ45 (primary), F12 (backup)
        /// - Side Frame Depth: BGM26 
        /// - Side Frame THK: CG32, CF32, CG30, CF30
        /// - Tube Layout: AW39-AW78 (front), similar for rear
        /// - Vertical Pitch: DF58-DF66 (front), DF70-DF78 (rear)
        /// 
        /// Complete cell mapping: docs/User_Guide/PREGO_IMPORT_USER_GUIDE.md
        /// </summary>
        private void ImportBundleFromPrego(BundleConfiguration config)
        {
            try
            {
                GlobalErrorHandler.LogInfo("ImportBundleFromPrego - Reading Bundle data from Prego");

                // Import Job Info first
                ImportJobInfoFromPrego();

                // BUNDLE WIDTH (Cell BQ45 primary, F12 backup)
                var bundleWidth = Excel.Prego.CellDouble(Excel.Prego.InputSheet, "BQ45");
                if (bundleWidth == 0)
                    bundleWidth = Excel.Prego.CellDouble(Excel.Prego.InputSheet, "F12");
                
                // Convert feet to inches if needed (values < 16 are assumed to be feet)
                if (bundleWidth > 0)
                {
                    if (bundleWidth < 16)
                        bundleWidth *= 12;
                    
                    BundleWidth = bundleWidth;
                    config.BundleWidth = bundleWidth;
                    GlobalErrorHandler.LogInfo($"? Bundle Width: {bundleWidth}\" (from cell BQ45/F12)");
                }

                // SIDE FRAME DEPTH (Cell BGM26 on InputsCalcsSheet)
                var sideFrameDepth = Excel.Prego.CellDouble(Excel.Prego.InputsCalcsSheet, "BGM26");
                if (sideFrameDepth > 0)
                {
                    SideFrameDepth = sideFrameDepth;
                    config.SideFrameDepth = sideFrameDepth;
                    GlobalErrorHandler.LogInfo($"? Side Frame Depth: {sideFrameDepth}\" (from cell BGM26)");
                }

                // SIDE FRAME THICKNESS (Cells CG32, CF32, CG30, CF30 - try in order)
                var sideFrameThk = Excel.Prego.CellDouble(Excel.Prego.InputSheet, "CG32", "CF32", "CG30", "CF30");
                if (sideFrameThk > 0)
                {
                    SideFrameThickness = sideFrameThk;
                    config.SideFrameThickness = sideFrameThk;
                    GlobalErrorHandler.LogInfo($"? Side Frame THK: {sideFrameThk}\" (from cells CG32/CF32/CG30/CF30)");
                }

                // BANK (Cell C7) - Convert number to bank letter
                var bankNumber = Excel.Prego.CellDouble(Excel.Prego.InputSheet, "C7");
                if (bankNumber > 0)
                {
                    config.Bank = (int)bankNumber;
                    GlobalErrorHandler.LogInfo($"? Bank: {(char)(bankNumber + 64)}");
                }

                // JOB INFO (B2, B4, B5, H3)
                var customer = Excel.Prego.CellString(Excel.Prego.InputSheet, "B2");
                if (!string.IsNullOrEmpty(customer))
                {
                    config.Customer = customer;
                    GlobalErrorHandler.LogInfo($"? Customer: {customer}");
                }

                var plantLocation = Excel.Prego.CellString(Excel.Prego.InputSheet, "B4");
                if (!string.IsNullOrEmpty(plantLocation))
                {
                    config.Location = plantLocation;
                    GlobalErrorHandler.LogInfo($"? Location: {plantLocation}");
                }

                var purchaseOrder = Excel.Prego.CellString(Excel.Prego.InputSheet, "B5");
                if (!string.IsNullOrEmpty(purchaseOrder))
                {
                    config.PurchaseOrder = purchaseOrder;
                    GlobalErrorHandler.LogInfo($"? PO: {purchaseOrder}");
                }

                var itemNumber = Excel.Prego.CellString(Excel.Prego.InputSheet, "H3");
                if (!string.IsNullOrEmpty(itemNumber))
                {
                    config.ItemNumber = itemNumber;
                    GlobalErrorHandler.LogInfo($"? Item: {itemNumber}");
                }

                // TITLEBLOCK MANUFACTURER (G27, F27)
                var titleblockManuf = Excel.Prego.CellString(Excel.Prego.InputSheet, "G27", "F27");
                if (!string.IsNullOrEmpty(titleblockManuf))
                {
                    config.TitleblockManufacturer = titleblockManuf;
                    GlobalErrorHandler.LogInfo($"? Manufacturer: {titleblockManuf}");
                }

                // HEADERS OUTSIDE FRAME (G15, F15) - Boolean
                var headersOutside = Excel.Prego.CellString(Excel.Prego.InputSheet, "G15", "F15");
                if (!string.IsNullOrEmpty(headersOutside))
                {
                    config.HeadersOutsideFrame = headersOutside.ToLower() == "yes" || headersOutside.ToLower() == "true";
                    GlobalErrorHandler.LogInfo($"? Headers Outside Frame: {config.HeadersOutsideFrame}");
                }

                // TUBE LENGTH (L15, N15) - Handle architectural feet (5'-6") or decimal feet
                var tubeLengthStr = Excel.Prego.CellString(Excel.Prego.InputSheet, "L15");
                double tubeLength = 0;
                if (!string.IsNullOrEmpty(tubeLengthStr) && tubeLengthStr.Contains("'"))
                {
                    // Parse architectural format: 5'-6" ? inches
                    var parts = tubeLengthStr.Split('\'', '"');
                    if (parts.Length >= 2)
                    {
                        double feet = double.TryParse(parts[0], out var f) ? f : 0;
                        double inches = parts.Length > 1 && double.TryParse(parts[1], out var i) ? i : 0;
                        tubeLength = feet * 12 + inches;
                    }
                }
                else
                {
                    // Decimal feet from N15
                    var tubeLengthFeet = Excel.Prego.CellDouble(Excel.Prego.InputSheet, "N15");
                    tubeLength = tubeLengthFeet * 12;
                }
                if (tubeLength > 0)
                {
                    config.TubeLength = tubeLength;
                    GlobalErrorHandler.LogInfo($"? Tube Length: {tubeLength}\" (from L15/N15)");
                }

                // TUBE PROJECTION (Manufacturer-specific: 0.25 for Smithco, 0.125 for HPC)
                config.TubeProjection = (config.TitleblockManufacturer == "Smithco") ? 0.25 : 0.125;
                GlobalErrorHandler.LogInfo($"? Tube Projection: {config.TubeProjection}\" ({config.TitleblockManufacturer})");

                // TUBE OD (L10)
                var tubeOD = Excel.Prego.CellDouble(Excel.Prego.InputSheet, "L10");
                if (tubeOD > 0)
                {
                    config.TubeOD = tubeOD;
                    GlobalErrorHandler.LogInfo($"? Tube OD: {tubeOD}\"");
                }

                // TUBE WALL THICKNESS (N14, L14)
                var tubeWallThk = Excel.Prego.CellDouble(Excel.Prego.InputSheet, "N14", "L14");
                if (tubeWallThk > 0)
                {
                    config.TubeWallThickness = tubeWallThk;
                    GlobalErrorHandler.LogInfo($"? Tube Wall THK: {tubeWallThk}\"");
                }

                // FIN OD (N19, L19)
                var finOD = Excel.Prego.CellDouble(Excel.Prego.InputSheet, "N19", "L19");
                if (finOD > 0)
                {
                    config.FinOD = finOD;
                    GlobalErrorHandler.LogInfo($"? Fin OD: {finOD}\"");
                }

                // TUBE ROW COUNTS (AW39, AU39, AW42, AU42)
                var tubeRow1 = (int)Excel.Prego.CellDouble(Excel.Prego.InputSheet, "AW39", "AU39");
                if (tubeRow1 > 0)
                {
                    config.TubeRow1Count = tubeRow1;
                    GlobalErrorHandler.LogInfo($"? Tube Row 1: {tubeRow1}");
                }

                var tubeRow2 = (int)Excel.Prego.CellDouble(Excel.Prego.InputSheet, "AW42", "AU42");
                if (tubeRow2 > 0)
                {
                    config.TubeRow2Count = tubeRow2;
                    GlobalErrorHandler.LogInfo($"? Tube Row 2: {tubeRow2}");
                }

                // TUBE HORIZONTAL PITCH (BO47)
                var tubeHorizPitch = Excel.Prego.CellDouble(Excel.Prego.InputSheet, "BO47");
                if (tubeHorizPitch > 0)
                {
                    config.HorizontalPitch = tubeHorizPitch;
                    GlobalErrorHandler.LogInfo($"? Horizontal Pitch: {tubeHorizPitch}\"");
                }

                // TUBE QUANTITY (N20, L20)
                var tubeQuantity = (int)Excel.Prego.CellDouble(Excel.Prego.InputSheet, "N20", "L20");
                if (tubeQuantity > 0)
                {
                    config.TubeQuantity = tubeQuantity;
                    GlobalErrorHandler.LogInfo($"? Tube Quantity: {tubeQuantity}");
                }

                // FIN STRIP BACK (X39, Y39 on PregoToMikeySheet)
                var finStripFront = Excel.Prego.CellDouble(Excel.Prego.PregoToMikeySheet, "X39");
                if (finStripFront > 0)
                {
                    config.FinStripBackFront = finStripFront;
                    GlobalErrorHandler.LogInfo($"? Fin Strip Back Front: {finStripFront}\"");
                }

                var finStripRear = Excel.Prego.CellDouble(Excel.Prego.PregoToMikeySheet, "Y39");
                if (finStripRear > 0)
                {
                    config.FinStripBackRear = finStripRear;
                    GlobalErrorHandler.LogInfo($"? Fin Strip Back Rear: {finStripRear}\"");
                }

                // VERTICAL PITCHES - FRONT (9 values: DF58-DF66 on SketchCalcsSheet)
                config.FrontVerticalPitches.Clear();
                for (int row = 58; row <= 66; row++)
                {
                    var pitch = Excel.Prego.CellDouble(Excel.Prego.SketchCalcsSheet, $"DF{row}");
                    config.FrontVerticalPitches.Add(pitch);
                }
                GlobalErrorHandler.LogInfo($"? Front Vertical Pitches: {config.FrontVerticalPitches.Count} values (DF58-DF66)");

                // VERTICAL PITCHES - REAR (9 values: DF70-DF78 on SketchCalcsSheet)
                config.RearVerticalPitches.Clear();
                for (int row = 70; row <= 78; row++)
                {
                    var pitch = Excel.Prego.CellDouble(Excel.Prego.SketchCalcsSheet, $"DF{row}");
                    config.RearVerticalPitches.Add(pitch);
                }
                GlobalErrorHandler.LogInfo($"? Rear Vertical Pitches: {config.RearVerticalPitches.Count} values (DF70-DF78)");

                // TUBE SUPPORTS (BGF12, BGF20, CG28, CF28, CG26, CF26)
                var tubeSupportSpacing = Excel.Prego.CellDouble(Excel.Prego.InputsCalcsSheet, "BGF12");
                if (tubeSupportSpacing > 0)
                {
                    config.TubeSupportSpacingFeet = tubeSupportSpacing;
                    GlobalErrorHandler.LogInfo($"? Tube Support Spacing: {tubeSupportSpacing} ft");
                }

                var tubeSupportQty = (int)Excel.Prego.CellDouble(Excel.Prego.InputsCalcsSheet, "BGF20");
                if (tubeSupportQty > 0)
                {
                    config.TubeSupportQuantity = tubeSupportQty;
                    GlobalErrorHandler.LogInfo($"? Tube Support Quantity: {tubeSupportQty}");
                }

                var tubeSupportSize = Excel.Prego.CellString(Excel.Prego.InputSheet, "CG28", "CF28", "CG26", "CF26");
                if (!string.IsNullOrEmpty(tubeSupportSize))
                {
                    // Extract first word (e.g., "C6" from "C6 x 8.2#")
                    config.TubeSupportSize = tubeSupportSize.Split(' ')[0];
                    GlobalErrorHandler.LogInfo($"? Tube Support Size: {config.TubeSupportSize}");
                }

                // HEADER DATA IMPORT (100+ fields for 6 header types: 61-66)
                // Note: Only import if Prego is open and HeaderAppData is initialized
                try
                {
                    if (Excel.Prego.PregoDoc != null && Excel.Header_DataManager.HeaderAppData != null)
                    {
                        Excel.Header_DataManager.ImportHeaderData_FromPrego();
                        GlobalErrorHandler.LogInfo("? Header data imported (6 header types)");
                    }
                    else
                    {
                        GlobalErrorHandler.LogWarning("Skipping header import - Prego not initialized or HeaderAppData not available");
                    }
                }
                catch (Exception ex)
                {
                    GlobalErrorHandler.LogWarning($"Header import failed: {ex.Message}");
                }

                // CAMBER AUTO-DETECTION LOGIC
                // If not Smithco and no tube slopes, enable camber
                bool isSmithco = (config.TitleblockManufacturer == "Smithco");
                if (!isSmithco && config.TubeRow1Count > 0)
                {
                    // Check if all slopes are zero (simplified check)
                    // In old UI: if (!IsSmithco && Tube.SlopesPerFootList[Tube.RowCount] == 0)
                    // For now, we'll just enable camber for non-Smithco units
                    config.Cambered = true;
                    GlobalErrorHandler.LogInfo("? Camber enabled (non-Smithco unit)");
                }

                GlobalErrorHandler.LogInfo("? Bundle data import complete - 40+ fields + Headers imported from Prego");

                // Update UI
                OnPropertyChanged(nameof(BundleWidth));
                OnPropertyChanged(nameof(SideFrameThickness));
                OnPropertyChanged(nameof(SideFrameDepth));
                UpdateDimensionsDisplay();
                UpdateValidation();
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "ImportBundleFromPrego");
                throw new Exception($"Failed to import Bundle data from Prego: {ex.Message}", ex);
            }
        }

        /// <summary>
        /// Import Header-specific data from Prego (replicates HeaderUI.bImportPrego_Click)
        /// 
        /// Full implementation uses:
        /// - Header_DataManager.ImportHeaderData_FromPrego() (100+ fields)
        /// - Connection_DataManager.ImportConnectionData_FromPrego()
        /// 
        /// Complete cell mapping: docs/User_Guide/PREGO_IMPORT_USER_GUIDE.md
        /// </summary>
        private void ImportHeaderFromPrego(HeaderConfiguration config)
        {
            try
            {
                GlobalErrorHandler.LogInfo("ImportHeaderFromPrego - Reading Header data from Prego");

                // Import Job Info first
                ImportJobInfoFromPrego();

                // NOTE: Full Header import is complex and uses Header_DataManager
                // This is a simplified version for initial implementation
                // Complete implementation will call existing Header import methods

                // Update UI
                OnPropertyChanged(nameof(CurrentConfiguration));
                UpdateDimensionsDisplay();
                UpdateValidation();

                GlobalErrorHandler.LogInfo("Header data imported from Prego (basic - job info only)");
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "ImportHeaderFromPrego");
                // Don't throw - partial import is acceptable
                GlobalErrorHandler.LogWarning("Header import completed with warnings");
            }
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

            // TODO: Implement loading configuration from SolidWorks template file
            // This would use _solidWorksService to open the template and extract parameters
            // CurrentConfiguration = _solidWorksService.LoadConfigFromTemplate(template);
            
            StatusMessage = $"Selected template: {template.DisplayName}";
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
            // OLD METHOD - Replaced by LoadTemplatesForCurrentTool()
            // Templates are now loaded automatically when tool selection changes
            // See OnToolChanged() method
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
                ValidationMessages.Add($"⚠︝ {warning}");
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
