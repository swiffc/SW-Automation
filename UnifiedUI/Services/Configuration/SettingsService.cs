
using System;
using System.Collections.Generic;
using System.IO;
using Newtonsoft.Json;
using UnifiedUI.Models;
using UnifiedUI.Models.Configuration;
using FileTools.Infrastructure;

namespace UnifiedUI.Services.Configuration
{
    /// <summary>
    /// Implementation of settings service
    /// </summary>
    public class SettingsService : ISettingsService
    {
        private readonly string _settingsFilePath;
        private AppSettings _currentSettings;
        private readonly JsonSerializerSettings _jsonSettings;

        public SettingsService(string settingsFilePath = null)
        {
            _settingsFilePath = settingsFilePath ?? Path.Combine(
                Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData),
                "SolidWorksAutomation",
                "settings.json"
            );

            _jsonSettings = new JsonSerializerSettings
            {
                Formatting = Formatting.Indented,
                TypeNameHandling = TypeNameHandling.Auto,
                NullValueHandling = NullValueHandling.Include
            };

            // Load or create default settings
            _currentSettings = LoadSettingsFromFile() ?? new AppSettings();
        }

        public AppSettings GetSettings()
        {
            return _currentSettings;
        }

        public bool SaveSettings(AppSettings settings)
        {
            if (settings == null)
            {
                GlobalErrorHandler.LogError("Cannot save null settings");
                return false;
            }

            try
            {
                // Validate settings
                var validation = ValidateSettings(settings);
                if (!validation.IsValid)
                {
                    GlobalErrorHandler.LogError($"Settings validation failed: {string.Join(", ", validation.Errors)}");
                    return false;
                }

                settings.LastModified = DateTime.Now;
                _currentSettings = settings;

                // Ensure directory exists
                var directory = Path.GetDirectoryName(_settingsFilePath);
                if (!Directory.Exists(directory))
                {
                    Directory.CreateDirectory(directory);
                }

                // Save to file
                var json = JsonConvert.SerializeObject(settings, _jsonSettings);
                File.WriteAllText(_settingsFilePath, json);

                GlobalErrorHandler.LogInfo("Settings saved successfully");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error saving settings");
                return false;
            }
        }

        public AppSettings ResetToDefaults()
        {
            GlobalErrorHandler.LogInfo("Resetting settings to defaults");
            _currentSettings = new AppSettings();
            SaveSettings(_currentSettings);
            return _currentSettings;
        }

        public T GetSetting<T>(string key, T defaultValue = default)
        {
            try
            {
                if (_currentSettings.UserPreferences.TryGetValue(key, out var value))
                {
                    if (value is T typedValue)
                    {
                        return typedValue;
                    }
                    
                    // Try to convert
                    return (T)Convert.ChangeType(value, typeof(T));
                }
                
                return defaultValue;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error getting setting: {key}");
                return defaultValue;
            }
        }

        public bool SetSetting<T>(string key, T value)
        {
            try
            {
                _currentSettings.UserPreferences[key] = value.ToString();
                return SaveSettings(_currentSettings);
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error setting: {key}");
                return false;
            }
        }

        public bool ExportSettings(string filePath)
        {
            if (string.IsNullOrWhiteSpace(filePath))
            {
                return false;
            }

            try
            {
                var json = JsonConvert.SerializeObject(_currentSettings, _jsonSettings);
                File.WriteAllText(filePath, json);
                
                GlobalErrorHandler.LogInfo($"Settings exported to: {filePath}");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error exporting settings to: {filePath}");
                return false;
            }
        }

        public bool ImportSettings(string filePath)
        {
            if (string.IsNullOrWhiteSpace(filePath) || !File.Exists(filePath))
            {
                GlobalErrorHandler.LogError($"Settings file not found: {filePath}");
                return false;
            }

            try
            {
                var json = File.ReadAllText(filePath);
                var settings = JsonConvert.DeserializeObject<AppSettings>(json, _jsonSettings);
                
                if (settings != null)
                {
                    var validation = ValidateSettings(settings);
                    if (validation.IsValid)
                    {
                        _currentSettings = settings;
                        SaveSettings(_currentSettings);
                        
                        GlobalErrorHandler.LogInfo($"Settings imported from: {filePath}");
                        return true;
                    }
                    else
                    {
                        GlobalErrorHandler.LogError($"Imported settings validation failed: {string.Join(", ", validation.Errors)}");
                        return false;
                    }
                }
                
                return false;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error importing settings from: {filePath}");
                return false;
            }
        }

        public ValidationResult ValidateSettings(AppSettings settings)
        {
            var result = new ValidationResult { IsValid = true };

            if (settings == null)
            {
                result.IsValid = false;
                result.Errors.Add("Settings cannot be null");
                return result;
            }

            // Validate paths exist or can be created
            if (settings.Paths != null)
            {
                ValidatePath(settings.Paths.TemplatesPath, "Templates Path", result);
                ValidatePath(settings.Paths.WorkflowsPath, "Workflows Path", result);
                ValidatePath(settings.Paths.OutputPath, "Output Path", result);
                ValidatePath(settings.Paths.LogsPath, "Logs Path", result);
            }

            // Validate SolidWorks settings
            if (settings.SolidWorks != null)
            {
                if (settings.SolidWorks.DocumentTimeout <= 0)
                {
                    result.IsValid = false;
                    result.Errors.Add("Document timeout must be greater than 0");
                }

                if (settings.SolidWorks.RebuildTimeout <= 0)
                {
                    result.IsValid = false;
                    result.Errors.Add("Rebuild timeout must be greater than 0");
                }
            }

            // Validate workflow settings
            if (settings.Workflows != null)
            {
                if (settings.Workflows.MaxParallelWorkflows <= 0)
                {
                    result.IsValid = false;
                    result.Errors.Add("Max parallel workflows must be greater than 0");
                }

                if (settings.Workflows.DefaultRetryCount < 0)
                {
                    result.IsValid = false;
                    result.Errors.Add("Default retry count cannot be negative");
                }
            }

            return result;
        }

        private void ValidatePath(string path, string pathName, ValidationResult result)
        {
            if (string.IsNullOrWhiteSpace(path))
            {
                result.IsValid = false;
                result.Errors.Add($"{pathName} cannot be empty");
                return;
            }

            try
            {
                // Try to create the directory if it doesn't exist
                if (!Directory.Exists(path))
                {
                    Directory.CreateDirectory(path);
                }
            }
            catch (Exception ex)
            {
                result.IsValid = false;
                result.Errors.Add($"{pathName} is invalid or cannot be created: {ex.Message}");
            }
        }

        private AppSettings LoadSettingsFromFile()
        {
            try
            {
                if (File.Exists(_settingsFilePath))
                {
                    var json = File.ReadAllText(_settingsFilePath);
                    var settings = JsonConvert.DeserializeObject<AppSettings>(json, _jsonSettings);
                    
                    GlobalErrorHandler.LogInfo("Settings loaded successfully");
                    return settings;
                }
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error loading settings from file");
            }

            return null;
        }
    }
}
