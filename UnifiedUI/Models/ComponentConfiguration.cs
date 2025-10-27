using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Runtime.CompilerServices;

namespace UnifiedUI.Models
{
    /// <summary>
    /// Base configuration model for all component types
    /// </summary>
    public class ComponentConfiguration : INotifyPropertyChanged
    {
        public event PropertyChangedEventHandler PropertyChanged;

        protected virtual void OnPropertyChanged([CallerMemberName] string propertyName = null)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }

        public string ComponentType { get; set; }
        public string JobNumber { get; set; }
        public string SerialNumber { get; set; }
        public string PartPrefix { get; set; }
        public string Revision { get; set; }

        // Common dimensions (inches)
        public double Width { get; set; }
        public double Height { get; set; }
        public double Depth { get; set; }
        public double Length { get; set; }

        // Parameters dictionary for flexible storage
        public Dictionary<string, object> Parameters { get; set; }

        // Metadata
        public DateTime Created { get; set; }
        public DateTime Modified { get; set; }
        public string CreatedBy { get; set; }

        public ComponentConfiguration()
        {
            Parameters = new Dictionary<string, object>();
            Created = DateTime.Now;
            Modified = DateTime.Now;
        }

        // Helper methods to get/set parameters with type safety
        public T GetParameter<T>(string key, T defaultValue = default)
        {
            if (Parameters.TryGetValue(key, out var value))
            {
                try
                {
                    return (T)Convert.ChangeType(value, typeof(T));
                }
                catch
                {
                    return defaultValue;
                }
            }
            return defaultValue;
        }

        public void SetParameter(string key, object value)
        {
            Parameters[key] = value;
            Modified = DateTime.Now;
        }
    }

    /// <summary>
    /// Bundle-specific configuration
    /// </summary>
    public class BundleConfiguration : ComponentConfiguration
    {
        private double _bundleWidth;
        public double BundleWidth 
        { 
            get => _bundleWidth;
            set 
            { 
                if (_bundleWidth != value)
                {
                    _bundleWidth = value;
                    Width = value; // Sync to base property for dimension display
                    OnPropertyChanged();
                    OnPropertyChanged(nameof(Width));
                }
            }
        }

        private double _bundleDepth;
        public double BundleDepth 
        { 
            get => _bundleDepth;
            set 
            { 
                if (_bundleDepth != value)
                {
                    _bundleDepth = value;
                    Depth = value; // Sync to base property for dimension display
                    OnPropertyChanged();
                    OnPropertyChanged(nameof(Depth));
                }
            }
        }

        public double SideFrameThickness { get; set; }
        public double SideFrameDepth { get; set; }
        public bool HeadersOutsideFrame { get; set; }
        public double TubeLength { get; set; }
        public double TubeProjection { get; set; }
        public double TubeOD { get; set; }
        public double TubeWallThickness { get; set; }
        public double FinOD { get; set; }
        public int TubeRow1Count { get; set; }
        public int TubeRow2Count { get; set; }
        public double HorizontalPitch { get; set; }
        public double VerticalPitch { get; set; }
        public List<double> VerticalPitches { get; set; }

        public BundleConfiguration()
        {
            ComponentType = "Bundle";
            VerticalPitches = new List<double>();
        }
    }

    /// <summary>
    /// Header-specific configuration
    /// </summary>
    public class HeaderConfiguration : ComponentConfiguration
    {
        public string HeaderType { get; set; } = "61";
        public double BoxWidth { get; set; }
        public double BoxHeight { get; set; }
        public double BoxLength { get; set; }
        public double TubesheetThickness { get; set; }
        public double DesignPressure { get; set; }
        public double MAWP { get; set; }
        
        public HeaderConfiguration()
        {
            ComponentType = "Header";
        }
    }

    /// <summary>
    /// Hood-specific configuration
    /// </summary>
    public class HoodConfiguration : ComponentConfiguration
    {
        public int Bank { get; set; }
        public int Stacks { get; set; }
        public int DepthOption { get; set; } = 36; // 36, 42, or 48
        public double FanDiameter { get; set; }
        public int WindLoad { get; set; } = 50;
        public double ShiftStiffeners { get; set; }
        public double Adjust { get; set; }
        
        // Job information
        public string Customer { get; set; }
        public string Client { get; set; }
        public string Location { get; set; }
        public string PurchaseOrder { get; set; }
        public string ItemNumber { get; set; }
        public string Initials { get; set; }
        
        public HoodConfiguration()
        {
            ComponentType = "Hood";
        }
    }

    /// <summary>
    /// Walkway-specific configuration
    /// </summary>
    public class WalkwayConfiguration : ComponentConfiguration
    {
        // Walkway dimensions
        public int Bank { get; set; }
        public double PlenumCenterWidth { get; set; }
        public double FloorHeight { get; set; }
        public double RailHeight { get; set; }
        public double OffsetFromColumnCenter { get; set; }
        public double SupportCenterToEnd { get; set; }
        public string ColumnSize { get; set; } = "W8×31";
        public string MinimumStringerSize { get; set; } = "C6";
        public string RailPosition { get; set; } = "Left";
        
        // Platform
        public double PlatformWidth { get; set; }
        public double PlatformLength { get; set; }
        public double PlatformFloorHeight { get; set; }
        public string PlatformMinimumStringerSize { get; set; } = "C6";
        
        // Handrail
        public double HandrailLength { get; set; }
        public double HandrailHeight { get; set; }
        public double HandrailFloorHeight { get; set; }
        public string HandrailMinimumStringerSize { get; set; } = "C6";
        
        // Job information
        public string Customer { get; set; }
        public string Client { get; set; }
        public string Location { get; set; }
        public string PurchaseOrder { get; set; }
        public string ItemNumber { get; set; }
        public string Initials { get; set; }
        
        public WalkwayConfiguration()
        {
            ComponentType = "Walkway";
        }
    }

    /// <summary>
    /// MachineryMount-specific configuration
    /// </summary>
    public class MachineryMountConfiguration : ComponentConfiguration
    {
        public int Bank { get; set; }
        public string MountType { get; set; }
        public double MountWidth { get; set; }
        public double MountLength { get; set; }
        public double MountHeight { get; set; }
        public double LoadCapacity { get; set; }
        public string MaterialSpec { get; set; }
        
        // Job information
        public string Customer { get; set; }
        public string Client { get; set; }
        public string Location { get; set; }
        public string PurchaseOrder { get; set; }
        public string ItemNumber { get; set; }
        public string Initials { get; set; }
        
        public MachineryMountConfiguration()
        {
            ComponentType = "Machinery Mount";
        }
    }

    /// <summary>
    /// Plenum-specific configuration
    /// </summary>
    public class PlenumConfiguration : ComponentConfiguration
    {
        public int Bank { get; set; }
        public double PlenumWidth { get; set; }
        public double PlenumLength { get; set; }
        public double PlenumHeight { get; set; }
        public double PlenumDepth { get; set; }
        public string AccessType { get; set; }
        public bool IncludeDrainPan { get; set; }
        public string Material { get; set; } = "Galvanized";
        
        // Job information
        public string Customer { get; set; }
        public string Client { get; set; }
        public string Location { get; set; }
        public string PurchaseOrder { get; set; }
        public string ItemNumber { get; set; }
        public string Initials { get; set; }
        
        public PlenumConfiguration()
        {
            ComponentType = "Plenum";
        }
    }

    /// <summary>
    /// Structure-specific configuration
    /// </summary>
    public class StructureConfiguration : ComponentConfiguration
    {
        public int Bank { get; set; }
        public string StructureType { get; set; } // "XCH" or "Z"
        public double StructureWidth { get; set; }
        public double StructureHeight { get; set; }
        public double StructureLength { get; set; }
        public int BayCount { get; set; }
        public double BaySpacing { get; set; }
        public string ColumnType { get; set; }
        public string BeamType { get; set; }
        public bool IncludeBracing { get; set; }
        
        // Job information
        public string Customer { get; set; }
        public string Client { get; set; }
        public string Location { get; set; }
        public string PurchaseOrder { get; set; }
        public string ItemNumber { get; set; }
        public string Initials { get; set; }
        
        public StructureConfiguration()
        {
            ComponentType = "Structure";
        }
    }
}
