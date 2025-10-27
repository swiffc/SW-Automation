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
}
