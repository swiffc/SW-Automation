using System;
using System.IO;

namespace UnifiedUI.Models
{
    /// <summary>
    /// Represents a SolidWorks template file
    /// </summary>
    public class Template
    {
        /// <summary>
        /// Full path to the template file
        /// </summary>
        public string FilePath { get; set; }

        /// <summary>
        /// User-friendly display name
        /// </summary>
        public string DisplayName { get; set; }

        /// <summary>
        /// Template file name (with extension)
        /// </summary>
        public string FileName => Path.GetFileName(FilePath);

        /// <summary>
        /// Tool type this template belongs to
        /// </summary>
        public ToolType ParentTool { get; set; }

        /// <summary>
        /// Template category (e.g., "Single", "Combined", "Assembly")
        /// </summary>
        public string Category { get; set; }

        /// <summary>
        /// Optional job number extracted from filename (e.g., "21380" from "XCH_21380.SLDASM")
        /// </summary>
        public string JobNumber { get; set; }

        /// <summary>
        /// Whether this is a valid, existing template file
        /// </summary>
        public bool IsValid => !string.IsNullOrEmpty(FilePath) && File.Exists(FilePath);

        public Template()
        {
        }

        public Template(string filePath, ToolType parentTool)
        {
            FilePath = filePath;
            ParentTool = parentTool;
            ParseDisplayName();
        }

        /// <summary>
        /// Parse the filename to create a user-friendly display name
        /// </summary>
        private void ParseDisplayName()
        {
            if (string.IsNullOrEmpty(FilePath))
            {
                DisplayName = "Unknown Template";
                return;
            }

            var fileName = Path.GetFileNameWithoutExtension(FilePath);

            // Header Section Tool patterns: "000000_S01c-Header" ? "Header (S01c)"
            if (fileName.StartsWith("000000_"))
            {
                var parts = fileName.Replace("000000_", "").Split('-');
                if (parts.Length >= 2)
                {
                    var code = parts[0];  // e.g., "S01c"
                    var name = parts[1];  // e.g., "Header"
                    DisplayName = $"{name} ({code})";
                    Category = code.Contains("c") ? "Combined" : "Single";
                }
                else
                {
                    DisplayName = fileName.Replace("000000_", "");
                }
            }
            // XCH Structure Tool patterns: "XCH_21380" ? "XCH Structure (21380)"
            else if (fileName.StartsWith("XCH_"))
            {
                var suffix = fileName.Replace("XCH_", "");
                if (int.TryParse(suffix, out _))
                {
                    JobNumber = suffix;
                    DisplayName = $"XCH Structure ({suffix})";
                    Category = "Job-Specific";
                }
                else
                {
                    DisplayName = suffix.Replace("Assembly", "Standard Assembly")
                                       .Replace("_", " ");
                    Category = "Standard";
                }
            }
            // Z Structure Tool patterns: "ZST_Z_21380" ? "Z Structure (21380)"
            else if (fileName.StartsWith("ZST_Z_"))
            {
                var suffix = fileName.Replace("ZST_Z_", "");
                if (int.TryParse(suffix, out _))
                {
                    JobNumber = suffix;
                    DisplayName = $"Z Structure ({suffix})";
                    Category = "Job-Specific";
                }
                else
                {
                    DisplayName = suffix.Replace("Assembly", "Standard Assembly")
                                       .Replace("_", " ");
                    Category = "Standard";
                }
            }
            // Default: use filename as-is
            else
            {
                DisplayName = fileName.Replace("_", " ");
                Category = "Other";
            }
        }

        public override string ToString() => DisplayName;
    }
}
