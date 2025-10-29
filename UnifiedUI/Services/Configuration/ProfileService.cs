
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Newtonsoft.Json;
using UnifiedUI.Models.Configuration;
using FileTools.Infrastructure;

namespace UnifiedUI.Services.Configuration
{
    /// <summary>
    /// Implementation of profile service
    /// </summary>
    public class ProfileService : IProfileService
    {
        private readonly string _profilesFolder;
        private readonly ISettingsService _settingsService;
        private readonly JsonSerializerSettings _jsonSettings;

        public ProfileService(ISettingsService settingsService, string profilesFolder = null)
        {
            _settingsService = settingsService ?? throw new ArgumentNullException(nameof(settingsService));
            _profilesFolder = profilesFolder ?? Path.Combine(
                Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData),
                "SolidWorksAutomation",
                "Profiles"
            );

            if (!Directory.Exists(_profilesFolder))
            {
                Directory.CreateDirectory(_profilesFolder);
            }

            _jsonSettings = new JsonSerializerSettings
            {
                Formatting = Formatting.Indented,
                TypeNameHandling = TypeNameHandling.Auto,
                NullValueHandling = NullValueHandling.Ignore
            };

            // Create default profile if none exists
            EnsureDefaultProfile();
        }

        public List<SettingsProfile> GetAllProfiles()
        {
            var profiles = new List<SettingsProfile>();

            try
            {
                var files = Directory.GetFiles(_profilesFolder, "*.json");
                
                foreach (var file in files)
                {
                    try
                    {
                        var json = File.ReadAllText(file);
                        var profile = JsonConvert.DeserializeObject<SettingsProfile>(json, _jsonSettings);
                        
                        if (profile != null)
                        {
                            profiles.Add(profile);
                        }
                    }
                    catch (Exception ex)
                    {
                        GlobalErrorHandler.LogError(ex, $"Error loading profile: {file}");
                    }
                }

                GlobalErrorHandler.LogInfo($"Loaded {profiles.Count} profiles");
                return profiles.OrderBy(p => !p.IsDefault).ThenBy(p => p.Name).ToList();
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error loading profiles");
                return profiles;
            }
        }

        public SettingsProfile GetProfile(string profileId)
        {
            if (string.IsNullOrWhiteSpace(profileId))
            {
                return null;
            }

            try
            {
                var filePath = GetProfileFilePath(profileId);
                
                if (!File.Exists(filePath))
                {
                    return null;
                }

                var json = File.ReadAllText(filePath);
                var profile = JsonConvert.DeserializeObject<SettingsProfile>(json, _jsonSettings);
                
                return profile;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error loading profile: {profileId}");
                return null;
            }
        }

        public SettingsProfile GetDefaultProfile()
        {
            var profiles = GetAllProfiles();
            return profiles.FirstOrDefault(p => p.IsDefault);
        }

        public bool SaveProfile(SettingsProfile profile)
        {
            if (profile == null)
            {
                GlobalErrorHandler.LogError("Cannot save null profile");
                return false;
            }

            try
            {
                profile.ModifiedDate = DateTime.Now;
                
                var filePath = GetProfileFilePath(profile.Id);
                var json = JsonConvert.SerializeObject(profile, _jsonSettings);
                
                File.WriteAllText(filePath, json);
                
                GlobalErrorHandler.LogInfo($"Profile saved: {profile.Name} ({profile.Id})");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error saving profile: {profile.Name}");
                return false;
            }
        }

        public bool DeleteProfile(string profileId)
        {
            if (string.IsNullOrWhiteSpace(profileId))
            {
                return false;
            }

            try
            {
                var profile = GetProfile(profileId);
                
                if (profile == null)
                {
                    return false;
                }

                // Cannot delete default profile
                if (profile.IsDefault)
                {
                    GlobalErrorHandler.LogError("Cannot delete default profile");
                    return false;
                }

                var filePath = GetProfileFilePath(profileId);
                
                if (File.Exists(filePath))
                {
                    File.Delete(filePath);
                    GlobalErrorHandler.LogInfo($"Profile deleted: {profile.Name} ({profileId})");
                    return true;
                }
                
                return false;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error deleting profile: {profileId}");
                return false;
            }
        }

        public bool SetDefaultProfile(string profileId)
        {
            if (string.IsNullOrWhiteSpace(profileId))
            {
                return false;
            }

            try
            {
                var allProfiles = GetAllProfiles();
                
                // Clear all default flags
                foreach (var p in allProfiles)
                {
                    if (p.IsDefault)
                    {
                        p.IsDefault = false;
                        SaveProfile(p);
                    }
                }

                // Set new default
                var profile = GetProfile(profileId);
                if (profile != null)
                {
                    profile.IsDefault = true;
                    SaveProfile(profile);
                    
                    GlobalErrorHandler.LogInfo($"Default profile set: {profile.Name}");
                    return true;
                }
                
                return false;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error setting default profile: {profileId}");
                return false;
            }
        }

        public bool ApplyProfile(string profileId)
        {
            var profile = GetProfile(profileId);
            
            if (profile == null)
            {
                GlobalErrorHandler.LogError($"Profile not found: {profileId}");
                return false;
            }

            try
            {
                // Apply profile settings to current settings
                _settingsService.SaveSettings(profile.Settings);
                
                GlobalErrorHandler.LogInfo($"Profile applied: {profile.Name}");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error applying profile: {profile.Name}");
                return false;
            }
        }

        public SettingsProfile CreateProfileFromCurrentSettings(string name, string description)
        {
            if (string.IsNullOrWhiteSpace(name))
            {
                GlobalErrorHandler.LogError("Profile name is required");
                return null;
            }

            try
            {
                var currentSettings = _settingsService.GetSettings();
                
                var profile = new SettingsProfile
                {
                    Name = name,
                    Description = description,
                    CreatedBy = Environment.UserName,
                    Settings = currentSettings
                };

                if (SaveProfile(profile))
                {
                    GlobalErrorHandler.LogInfo($"Profile created: {name}");
                    return profile;
                }
                
                return null;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error creating profile: {name}");
                return null;
            }
        }

        public bool ExportProfile(string profileId, string filePath)
        {
            var profile = GetProfile(profileId);
            
            if (profile == null)
            {
                GlobalErrorHandler.LogError($"Profile not found: {profileId}");
                return false;
            }

            try
            {
                var json = JsonConvert.SerializeObject(profile, _jsonSettings);
                File.WriteAllText(filePath, json);
                
                GlobalErrorHandler.LogInfo($"Profile exported: {profile.Name} to {filePath}");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error exporting profile: {profile.Name}");
                return false;
            }
        }

        public SettingsProfile ImportProfile(string filePath)
        {
            if (string.IsNullOrWhiteSpace(filePath) || !File.Exists(filePath))
            {
                GlobalErrorHandler.LogError($"Profile file not found: {filePath}");
                return null;
            }

            try
            {
                var json = File.ReadAllText(filePath);
                var profile = JsonConvert.DeserializeObject<SettingsProfile>(json, _jsonSettings);
                
                if (profile != null)
                {
                    // Generate new ID and update dates
                    profile.Id = Guid.NewGuid().ToString();
                    profile.CreatedDate = DateTime.Now;
                    profile.ModifiedDate = DateTime.Now;
                    profile.IsDefault = false;

                    if (SaveProfile(profile))
                    {
                        GlobalErrorHandler.LogInfo($"Profile imported: {profile.Name}");
                        return profile;
                    }
                }
                
                return null;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error importing profile from: {filePath}");
                return null;
            }
        }

        private string GetProfileFilePath(string profileId)
        {
            return Path.Combine(_profilesFolder, $"{profileId}.json");
        }

        private void EnsureDefaultProfile()
        {
            var profiles = GetAllProfiles();
            
            if (!profiles.Any())
            {
                // Create default profile
                var defaultProfile = new SettingsProfile
                {
                    Name = "Default",
                    Description = "Default application settings",
                    IsDefault = true,
                    CreatedBy = Environment.UserName,
                    Settings = new AppSettings()
                };

                SaveProfile(defaultProfile);
                GlobalErrorHandler.LogInfo("Default profile created");
            }
        }
    }
}
