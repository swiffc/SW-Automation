
using System.Collections.Generic;
using UnifiedUI.Models.Configuration;

namespace UnifiedUI.Services.Configuration
{
    /// <summary>
    /// Interface for managing settings profiles
    /// </summary>
    public interface IProfileService
    {
        /// <summary>
        /// Gets all available profiles
        /// </summary>
        /// <returns>List of profiles</returns>
        List<SettingsProfile> GetAllProfiles();

        /// <summary>
        /// Gets a profile by ID
        /// </summary>
        /// <param name="profileId">Profile ID</param>
        /// <returns>Profile or null</returns>
        SettingsProfile GetProfile(string profileId);

        /// <summary>
        /// Gets the default profile
        /// </summary>
        /// <returns>Default profile or null</returns>
        SettingsProfile GetDefaultProfile();

        /// <summary>
        /// Saves a profile
        /// </summary>
        /// <param name="profile">Profile to save</param>
        /// <returns>True if successful</returns>
        bool SaveProfile(SettingsProfile profile);

        /// <summary>
        /// Deletes a profile
        /// </summary>
        /// <param name="profileId">Profile ID</param>
        /// <returns>True if successful</returns>
        bool DeleteProfile(string profileId);

        /// <summary>
        /// Sets a profile as default
        /// </summary>
        /// <param name="profileId">Profile ID</param>
        /// <returns>True if successful</returns>
        bool SetDefaultProfile(string profileId);

        /// <summary>
        /// Applies a profile (loads its settings)
        /// </summary>
        /// <param name="profileId">Profile ID</param>
        /// <returns>True if successful</returns>
        bool ApplyProfile(string profileId);

        /// <summary>
        /// Creates a new profile from current settings
        /// </summary>
        /// <param name="name">Profile name</param>
        /// <param name="description">Profile description</param>
        /// <returns>Created profile or null</returns>
        SettingsProfile CreateProfileFromCurrentSettings(string name, string description);

        /// <summary>
        /// Exports a profile to file
        /// </summary>
        /// <param name="profileId">Profile ID</param>
        /// <param name="filePath">File path</param>
        /// <returns>True if successful</returns>
        bool ExportProfile(string profileId, string filePath);

        /// <summary>
        /// Imports a profile from file
        /// </summary>
        /// <param name="filePath">File path</param>
        /// <returns>Imported profile or null</returns>
        SettingsProfile ImportProfile(string filePath);
    }
}
