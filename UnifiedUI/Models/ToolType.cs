using System;
using System.Collections.Generic;
using System.IO;
using System.Windows.Media;

namespace UnifiedUI.Models
{
    /// <summary>
    /// Represents a tool/project type in the application
    /// Based on actual project structure: 4 tools with different configurations
    /// </summary>
    public class ToolType
    {
        #region Properties

        public string Id { get; set; }
        public string Name { get; set; }
        public string Description { get; set; }
        public string Icon { get; set; } // Emoji icon for UI display
        public string IconPath { get; set; }
        public string TemplatePath { get; set; }
        public string ExcelConfigPath { get; set; }
        public List<string> AvailableComponents { get; set; }
        public int ExcelConfigCount { get; set; }
        public int TemplateFileCount { get; set; }
        public Brush AccentColor { get; set; }

        #endregion

        #region Static Tool Definitions

        /// <summary>
        /// Header Section Tool - Main tool with 37 Excel configurations
        /// 95 CAD template files, 779 MB
        /// Full component suite available
        /// </summary>
        public static ToolType HeaderSectionTool => new ToolType
        {
            Id = "HeaderSection",
            Name = "Header Section Tool",
            Description = "Main tool with 37 Excel configurations (95 templates, 779 MB)",
            Icon = "??",
            TemplatePath = Path.Combine("templates", "header_section_tool"),
            ExcelConfigPath = Path.Combine("templates", "header_section_tool"),
            ExcelConfigCount = 37,
            TemplateFileCount = 95,
            AvailableComponents = new List<string>
            {
                "Bundle",
                "Header",
                "Hood",
                "Machinery Mount",
                "Plenum",
                "Structure",
                "Walkway"
            },
            AccentColor = new SolidColorBrush(Color.FromRgb(52, 152, 219)) // Blue
        };

        /// <summary>
        /// XCH Structure Tool - Cross-flow structures
        /// 1 Excel config (XCH_SCS.xlsx), 308 CAD files, 476 MB
        /// Single component focus
        /// </summary>
        public static ToolType XCHStructureTool => new ToolType
        {
            Id = "XCHStructure",
            Name = "XCH Structure Tool",
            Description = "Cross-flow structures (1 config, 308 templates, 476 MB)",
            Icon = "??",
            TemplatePath = Path.Combine("templates", "xch_structure_tool"),
            ExcelConfigPath = Path.Combine("templates", "xch_structure_tool", "XCH_SCS.xlsx"),
            ExcelConfigCount = 1,
            TemplateFileCount = 308,
            AvailableComponents = new List<string>
            {
                "XCH Structure"
            },
            AccentColor = new SolidColorBrush(Color.FromRgb(46, 204, 113)) // Green
        };

        /// <summary>
        /// Z Structure Tool - Vertical structures
        /// 1 Excel config (Lifting System), 1,198 CAD files (HUGE!), 1,275 MB
        /// Single component with lazy loading required
        /// </summary>
        public static ToolType ZStructureTool => new ToolType
        {
            Id = "ZStructure",
            Name = "Z Structure Tool",
            Description = "Vertical structures (1 config, 1,198 templates, 1.2 GB)",
            Icon = "??",
            TemplatePath = Path.Combine("templates", "z_structure_tool"),
            ExcelConfigPath = Path.Combine("templates", "z_structure_tool", "Lifting System Work Sheet.xlsx"),
            ExcelConfigCount = 1,
            TemplateFileCount = 1198,
            AvailableComponents = new List<string>
            {
                "Z Structure"
            },
            AccentColor = new SolidColorBrush(Color.FromRgb(155, 89, 182)) // Purple
        };

        /// <summary>
        /// Hudson Certified - Separate project with different workflow
        /// 1 Excel config (Fan Calculator), 172 CAD files, 59 MB
        /// Unique workflow, may need separate view
        /// </summary>
        public static ToolType HudsonCertified => new ToolType
        {
            Id = "HudsonCertified",
            Name = "Hudson Certified",
            Description = "Certified drawings (1 config, 172 templates, 59 MB)",
            Icon = "?",
            TemplatePath = Path.Combine("templates", "hudson_certified"),
            ExcelConfigPath = Path.Combine("templates", "hudson_certified", "Cofimco Fan Calculator.xlsx"),
            ExcelConfigCount = 1,
            TemplateFileCount = 172,
            AvailableComponents = new List<string>
            {
                "Certified Drawing"
            },
            AccentColor = new SolidColorBrush(Color.FromRgb(230, 126, 34)) // Orange
        };

        /// <summary>
        /// All available tools in the application
        /// </summary>
        public static List<ToolType> AllTools => new List<ToolType>
        {
            HeaderSectionTool,
            XCHStructureTool,
            ZStructureTool,
            HudsonCertified
        };

        #endregion

        #region Helper Methods

        /// <summary>
        /// Get full template path relative to project root
        /// </summary>
        public string GetFullTemplatePath()
        {
            var projectRoot = Path.Combine(
                Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),
                "CascadeProjects",
                "Solidworks_Automation"
            );
            return Path.Combine(projectRoot, TemplatePath);
        }

        /// <summary>
        /// Get full Excel config path relative to project root
        /// </summary>
        public string GetFullExcelPath()
        {
            var projectRoot = Path.Combine(
                Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),
                "CascadeProjects",
                "Solidworks_Automation"
            );
            return Path.Combine(projectRoot, ExcelConfigPath);
        }

        /// <summary>
        /// Check if this tool has multiple Excel configurations
        /// </summary>
        public bool HasMultipleConfigs()
        {
            return ExcelConfigCount > 1;
        }

        /// <summary>
        /// Check if this tool requires lazy loading (large template count)
        /// </summary>
        public bool RequiresLazyLoading()
        {
            // Z Structure with 1,198 templates requires lazy loading
            return TemplateFileCount > 500;
        }

        /// <summary>
        /// Check if this tool has unique workflow (Hudson Certified)
        /// </summary>
        public bool HasUniqueWorkflow()
        {
            return Id == "HudsonCertified";
        }

        #endregion

        #region Overrides

        public override string ToString()
        {
            return $"{Name} ({ExcelConfigCount} configs, {TemplateFileCount} templates)";
        }

        public override bool Equals(object obj)
        {
            if (obj is ToolType other)
            {
                return Id == other.Id;
            }
            return false;
        }

        public override int GetHashCode()
        {
            return Id?.GetHashCode() ?? 0;
        }

        #endregion
    }
}

