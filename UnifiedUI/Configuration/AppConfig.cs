using System;
using System.IO;

namespace UnifiedUI.Configuration
{
    /// <summary>
    /// Application configuration and path management
    /// Centralizes all path configuration to eliminate hardcoded values
    /// </summary>
    public static class AppConfig
    {
        #region Base Paths

        /// <summary>
        /// Gets the project root directory
        /// Default: {UserProfile}/CascadeProjects/Solidworks_Automation
        /// </summary>
        public static string ProjectRoot => Path.Combine(
            Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),
            "CascadeProjects",
            "Solidworks_Automation"
        );

        /// <summary>
        /// Gets the templates root directory
        /// </summary>
        public static string TemplatesRoot => Path.Combine(ProjectRoot, "templates");

        /// <summary>
        /// Gets the output root directory
        /// </summary>
        public static string OutputRoot => Path.Combine(ProjectRoot, "output");

        /// <summary>
        /// Gets the macros root directory
        /// </summary>
        public static string MacrosRoot => Path.Combine(ProjectRoot, "macros");

        /// <summary>
        /// Gets the documentation root directory
        /// </summary>
        public static string DocsRoot => Path.Combine(ProjectRoot, "docs");

        #endregion

        #region Tool-Specific Paths

        /// <summary>
        /// Gets the Header Section Tool template path
        /// </summary>
        public static string HeaderSectionToolPath => Path.Combine(TemplatesRoot, "header_section_tool");

        /// <summary>
        /// Gets the XCH Structure Tool template path
        /// </summary>
        public static string XCHStructureToolPath => Path.Combine(TemplatesRoot, "xch_structure_tool");

        /// <summary>
        /// Gets the Z Structure Tool template path
        /// </summary>
        public static string ZStructureToolPath => Path.Combine(TemplatesRoot, "z_structure_tool");

        /// <summary>
        /// Gets the Hudson Certified template path
        /// </summary>
        public static string HudsonCertifiedPath => Path.Combine(TemplatesRoot, "hudson_certified");

        #endregion

        #region Prego Import Paths

        /// <summary>
        /// Gets the Prego archive path for a specific job
        /// Format: {OutputRoot}/{Job}/Drafting/Headers/~Archive/{Job}-prego{Bank}.xlsm
        /// </summary>
        /// <param name="jobNumber">Job number (e.g., "S2XXXX")</param>
        /// <param name="bank">Bank letter (e.g., "A")</param>
        /// <returns>Full path to Prego file</returns>
        public static string GetPregoPath(string jobNumber, char bank)
        {
            return Path.Combine(
                OutputRoot,
                jobNumber,
                "Drafting",
                "Headers",
                "~Archive",
                $"{jobNumber}-prego{bank}.xlsm"
            );
        }

        /// <summary>
        /// Gets the job output directory
        /// </summary>
        /// <param name="jobNumber">Job number (e.g., "S2XXXX")</param>
        /// <returns>Full path to job directory</returns>
        public static string GetJobOutputPath(string jobNumber)
        {
            return Path.Combine(OutputRoot, jobNumber);
        }

        #endregion

        #region Alternate Search Paths

        /// <summary>
        /// Gets alternate search paths for templates and Excel files
        /// Useful for file dialogs and recent files
        /// </summary>
        public static string[] GetSearchPaths()
        {
            return new[]
            {
                ProjectRoot,
                TemplatesRoot,
                OutputRoot,
                Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), "Documents"),
                @"C:\Jobs" // Common alternate location
            };
        }

        #endregion

        #region Validation

        /// <summary>
        /// Validates that essential paths exist
        /// </summary>
        /// <returns>True if all essential paths exist</returns>
        public static bool ValidatePaths()
        {
            return Directory.Exists(ProjectRoot) &&
                   Directory.Exists(TemplatesRoot);
        }

        /// <summary>
        /// Creates necessary directories if they don't exist
        /// </summary>
        public static void EnsureDirectoriesExist()
        {
            Directory.CreateDirectory(ProjectRoot);
            Directory.CreateDirectory(TemplatesRoot);
            Directory.CreateDirectory(OutputRoot);
            Directory.CreateDirectory(MacrosRoot);
            Directory.CreateDirectory(DocsRoot);
        }

        #endregion

        #region File Filters

        /// <summary>
        /// Excel file filter for file dialogs
        /// </summary>
        public static string ExcelFileFilter => "Excel Files (*.xlsm;*.xlsx;*.xls)|*.xlsm;*.xlsx;*.xls|All Files (*.*)|*.*";

        /// <summary>
        /// SolidWorks assembly file filter
        /// </summary>
        public static string SolidWorksAssemblyFilter => "SolidWorks Assemblies (*.SLDASM)|*.SLDASM|All Files (*.*)|*.*";

        /// <summary>
        /// SolidWorks part file filter
        /// </summary>
        public static string SolidWorksPartFilter => "SolidWorks Parts (*.SLDPRT)|*.SLDPRT|All Files (*.*)|*.*";

        /// <summary>
        /// SolidWorks drawing file filter
        /// </summary>
        public static string SolidWorksDrawingFilter => "SolidWorks Drawings (*.SLDDRW)|*.SLDDRW|All Files (*.*)|*.*";

        #endregion

        #region Application Settings

        /// <summary>
        /// Application name
        /// </summary>
        public static string ApplicationName => "UnifiedUI - SolidWorks Automation Suite";

        /// <summary>
        /// Application version
        /// </summary>
        public static string Version => "1.1.0";

        /// <summary>
        /// Default user initials (can be overridden)
        /// </summary>
        public static string DefaultInitials => "DC";

        /// <summary>
        /// Default titleblock manufacturer
        /// </summary>
        public static string DefaultManufacturer => "HPC";

        #endregion
    }
}
