using System;

namespace UnifiedUI.Models
{
    /// <summary>
    /// Template for quick configuration loading
    /// </summary>
    public class Template
    {
        public string Id { get; set; }
        public string Name { get; set; }
        public string Description { get; set; }
        public string ComponentType { get; set; }
        public DateTime Created { get; set; }
        public DateTime LastUsed { get; set; }
        public int UsageCount { get; set; }
        public string FilePath { get; set; }
        public ComponentConfiguration Configuration { get; set; }

        public Template()
        {
            Id = Guid.NewGuid().ToString();
            Created = DateTime.Now;
            LastUsed = DateTime.Now;
            UsageCount = 0;
        }
    }
}
