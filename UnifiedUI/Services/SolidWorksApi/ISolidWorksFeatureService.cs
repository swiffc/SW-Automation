
using System.Collections.Generic;
using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;

namespace UnifiedUI.Services.SolidWorksApi
{
    /// <summary>
    /// Interface for SolidWorks feature manipulation operations
    /// </summary>
    public interface ISolidWorksFeatureService
    {
        /// <summary>
        /// Gets all features in the active document
        /// </summary>
        /// <returns>List of features</returns>
        List<Feature> GetAllFeatures();

        /// <summary>
        /// Gets a feature by name
        /// </summary>
        /// <param name="featureName">Name of the feature</param>
        /// <returns>Feature or null if not found</returns>
        Feature GetFeatureByName(string featureName);

        /// <summary>
        /// Suppresses a feature
        /// </summary>
        /// <param name="featureName">Name of the feature to suppress</param>
        /// <returns>True if successful</returns>
        bool SuppressFeature(string featureName);

        /// <summary>
        /// Unsuppresses a feature
        /// </summary>
        /// <param name="featureName">Name of the feature to unsuppress</param>
        /// <returns>True if successful</returns>
        bool UnsuppressFeature(string featureName);

        /// <summary>
        /// Deletes a feature
        /// </summary>
        /// <param name="featureName">Name of the feature to delete</param>
        /// <returns>True if successful</returns>
        bool DeleteFeature(string featureName);

        /// <summary>
        /// Renames a feature
        /// </summary>
        /// <param name="oldName">Current feature name</param>
        /// <param name="newName">New feature name</param>
        /// <returns>True if successful</returns>
        bool RenameFeature(string oldName, string newName);

        /// <summary>
        /// Edits a dimension value
        /// </summary>
        /// <param name="dimensionName">Name of the dimension</param>
        /// <param name="value">New value</param>
        /// <returns>True if successful</returns>
        bool EditDimension(string dimensionName, double value);

        /// <summary>
        /// Creates a sketch on a specified plane
        /// </summary>
        /// <param name="planeName">Name of the plane (Front, Top, Right, or custom)</param>
        /// <returns>Created sketch feature or null</returns>
        Feature CreateSketch(string planeName);

        /// <summary>
        /// Creates an extrude feature from sketch
        /// </summary>
        /// <param name="sketchName">Name of the sketch</param>
        /// <param name="depth">Extrude depth</param>
        /// <param name="reverse">Reverse direction</param>
        /// <returns>Created feature or null</returns>
        Feature CreateExtrude(string sketchName, double depth, bool reverse = false);
    }
}
