
using System;
using System.Collections.Generic;

namespace UnifiedUI.Models.Configuration
{
    /// <summary>
    /// Settings profile for saving/loading different configurations
    /// </summary>
    public class SettingsProfile
    {
        public string Id { get; set; }
        public string Name { get; set; }
        public string Description { get; set; }
        public DateTime CreatedDate { get; set; }
        public DateTime ModifiedDate { get; set; }
        public string CreatedBy { get; set; }
        public bool IsDefault { get; set; }
        public AppSettings Settings { get; set; }
        public Dictionary<string, object> Metadata { get; set; }

        public SettingsProfile()
        {
            Id = Guid.NewGuid().ToString();
            CreatedDate = DateTime.Now;
            ModifiedDate = DateTime.Now;
            Settings = new AppSettings();
            Metadata = new Dictionary<string, object>();
        }

        /// <summary>
        /// Creates a deep copy of the profile
        /// </summary>
        public SettingsProfile Clone()
        {
            var json = Newtonsoft.Json.JsonConvert.SerializeObject(this);
            var clone = Newtonsoft.Json.JsonConvert.DeserializeObject<SettingsProfile>(json);
            clone.Id = Guid.NewGuid().ToString();
            clone.CreatedDate = DateTime.Now;
            clone.ModifiedDate = DateTime.Now;
            clone.IsDefault = false;
            return clone;
        }
    }
}
