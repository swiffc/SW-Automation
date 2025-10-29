
using System.Collections.Generic;
using UnifiedUI.Models.Configuration;

namespace UnifiedUI.Services.Configuration
{
    /// <summary>
    /// Interface for managing application settings
    /// </summary>
    public interface ISettingsService
    {
        /// <summary>
        /// Gets the current application settings
        /// </summary>
        AppSettings GetSettings();

        /// <summary>
        /// Saves the current settings
        /// </summary>
        /// <param name="settings">Settings to save</param>
        /// <returns>True if successful</returns>
        bool SaveSettings(AppSettings settings);

        /// <summary>
        /// Resets settings to defaults
        /// </summary>
        /// <returns>Default settings</returns>
        AppSettings ResetToDefaults();

        /// <summary>
        /// Gets a specific setting value
        /// </summary>
        /// <typeparam name="T">Type of the setting</typeparam>
        /// <param name="key">Setting key</param>
        /// <param name="defaultValue">Default value if not found</param>
        /// <returns>Setting value</returns>
        T GetSetting<T>(string key, T defaultValue = default);

        /// <summary>
        /// Sets a specific setting value
        /// </summary>
        /// <typeparam name="T">Type of the setting</typeparam>
        /// <param name="key">Setting key</param>
        /// <param name="value">Value to set</param>
        /// <returns>True if successful</returns>
        bool SetSetting<T>(string key, T value);

        /// <summary>
        /// Exports settings to a file
        /// </summary>
        /// <param name="filePath">File path</param>
        /// <returns>True if successful</returns>
        bool ExportSettings(string filePath);

        /// <summary>
        /// Imports settings from a file
        /// </summary>
        /// <param name="filePath">File path</param>
        /// <returns>True if successful</returns>
        bool ImportSettings(string filePath);

        /// <summary>
        /// Validates settings
        /// </summary>
        /// <param name="settings">Settings to validate</param>
        /// <returns>Validation result</returns>
        ValidationResult ValidateSettings(AppSettings settings);
    }
}
