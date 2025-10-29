
using System.Collections.Generic;

namespace UnifiedUI.Services.SolidWorksApi
{
    /// <summary>
    /// Interface for SolidWorks custom property operations
    /// </summary>
    public interface ISolidWorksPropertyService
    {
        /// <summary>
        /// Gets a custom property value
        /// </summary>
        /// <param name="propertyName">Name of the property</param>
        /// <param name="configName">Configuration name (empty for file-level properties)</param>
        /// <returns>Property value or empty string if not found</returns>
        string GetProperty(string propertyName, string configName = "");

        /// <summary>
        /// Sets a custom property value
        /// </summary>
        /// <param name="propertyName">Name of the property</param>
        /// <param name="value">Value to set</param>
        /// <param name="configName">Configuration name (empty for file-level properties)</param>
        /// <returns>True if successful</returns>
        bool SetProperty(string propertyName, string value, string configName = "");

        /// <summary>
        /// Deletes a custom property
        /// </summary>
        /// <param name="propertyName">Name of the property to delete</param>
        /// <param name="configName">Configuration name (empty for file-level properties)</param>
        /// <returns>True if successful</returns>
        bool DeleteProperty(string propertyName, string configName = "");

        /// <summary>
        /// Gets all custom properties
        /// </summary>
        /// <param name="configName">Configuration name (empty for file-level properties)</param>
        /// <returns>Dictionary of property names and values</returns>
        Dictionary<string, string> GetAllProperties(string configName = "");

        /// <summary>
        /// Sets multiple properties at once
        /// </summary>
        /// <param name="properties">Dictionary of property names and values</param>
        /// <param name="configName">Configuration name (empty for file-level properties)</param>
        /// <returns>True if all successful</returns>
        bool SetMultipleProperties(Dictionary<string, string> properties, string configName = "");

        /// <summary>
        /// Clears all custom properties
        /// </summary>
        /// <param name="configName">Configuration name (empty for file-level properties)</param>
        /// <returns>True if successful</returns>
        bool ClearAllProperties(string configName = "");
    }
}
