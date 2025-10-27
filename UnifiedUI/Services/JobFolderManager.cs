using System;
using System.IO;

namespace UnifiedUI.Services
{
    /// <summary>
    /// Manages job folder structure and file organization
    /// </summary>
    public class JobFolderManager
    {
        private readonly string _jobsBasePath;

        public JobFolderManager(string jobsBasePath = null)
        {
            // Default to C:\Jobs\ or configurable path
            _jobsBasePath = jobsBasePath ?? @"C:\Jobs";
        }

        /// <summary>
        /// Create complete folder structure for a job
        /// </summary>
        public JobFolderResult CreateJobFolderStructure(string jobNumber, string componentType, string variant = null)
        {
            var result = new JobFolderResult { Success = true, JobNumber = jobNumber };

            try
            {
                // Create main job folder
                result.JobFolderPath = Path.Combine(_jobsBasePath, jobNumber);
                Directory.CreateDirectory(result.JobFolderPath);

                // Create component-specific subfolders
                switch (componentType)
                {
                    case "Bundle":
                        result.ComponentFolderPath = Path.Combine(result.JobFolderPath, "Bundle");
                        CreateBundleFolders(result.ComponentFolderPath);
                        break;

                    case "Header":
                        if (!string.IsNullOrEmpty(variant))
                        {
                            result.ComponentFolderPath = Path.Combine(result.JobFolderPath, "Headers", variant);
                        }
                        else
                        {
                            result.ComponentFolderPath = Path.Combine(result.JobFolderPath, "Headers");
                        }
                        CreateHeaderFolders(result.ComponentFolderPath);
                        break;

                    case "XCH Structure":
                        result.ComponentFolderPath = Path.Combine(result.JobFolderPath, "Structure", "XCH");
                        CreateStructureFolders(result.ComponentFolderPath);
                        break;

                    case "Z Structure":
                        result.ComponentFolderPath = Path.Combine(result.JobFolderPath, "Structure", "Z");
                        CreateStructureFolders(result.ComponentFolderPath);
                        break;

                    case "Hood":
                        result.ComponentFolderPath = Path.Combine(result.JobFolderPath, "Hood");
                        CreateHoodFolders(result.ComponentFolderPath);
                        break;

                    case "Plenum":
                        result.ComponentFolderPath = Path.Combine(result.JobFolderPath, "Plenum");
                        Directory.CreateDirectory(result.ComponentFolderPath);
                        break;

                    default:
                        result.ComponentFolderPath = Path.Combine(result.JobFolderPath, componentType);
                        Directory.CreateDirectory(result.ComponentFolderPath);
                        break;
                }

                result.Message = $"Created job folder structure at: {result.ComponentFolderPath}";
            }
            catch (Exception ex)
            {
                result.Success = false;
                result.Error = $"Error creating folders: {ex.Message}";
            }

            return result;
        }

        private void CreateBundleFolders(string basePath)
        {
            Directory.CreateDirectory(basePath);
            Directory.CreateDirectory(Path.Combine(basePath, "Parts"));
            Directory.CreateDirectory(Path.Combine(basePath, "Drawings"));
            Directory.CreateDirectory(Path.Combine(basePath, "Assembly"));
        }

        private void CreateHeaderFolders(string basePath)
        {
            Directory.CreateDirectory(basePath);
            Directory.CreateDirectory(Path.Combine(basePath, "Parts"));
            Directory.CreateDirectory(Path.Combine(basePath, "Assemblies"));
            Directory.CreateDirectory(Path.Combine(basePath, "Drawings"));
            Directory.CreateDirectory(Path.Combine(basePath, "Config"));
            Directory.CreateDirectory(Path.Combine(basePath, "DesignTables"));
        }

        private void CreateStructureFolders(string basePath)
        {
            Directory.CreateDirectory(basePath);
            Directory.CreateDirectory(Path.Combine(basePath, "Parts"));
            Directory.CreateDirectory(Path.Combine(basePath, "Assemblies"));
            Directory.CreateDirectory(Path.Combine(basePath, "Drawings"));
            Directory.CreateDirectory(Path.Combine(basePath, "Calculations"));
        }

        private void CreateHoodFolders(string basePath)
        {
            Directory.CreateDirectory(basePath);
            Directory.CreateDirectory(Path.Combine(basePath, "Parts"));
            Directory.CreateDirectory(Path.Combine(basePath, "Drawings"));
        }

        /// <summary>
        /// Get standard file naming for component
        /// </summary>
        public string GetStandardFileName(string jobNumber, string componentType, string fileType, string variant = null)
        {
            return componentType switch
            {
                "Bundle" => $"{jobNumber}-7.{fileType}", // Bundle is always -7
                "Header" when !string.IsNullOrEmpty(variant) => $"{jobNumber}_{variant}-Header.{fileType}",
                "Header" => $"{jobNumber}-61.{fileType}", // Default header
                "XCH Structure" => $"{jobNumber}_XCH.{fileType}",
                "Z Structure" => $"{jobNumber}_Z.{fileType}",
                _ => $"{jobNumber}_{componentType}.{fileType}"
            };
        }

        /// <summary>
        /// Check if job folder already exists
        /// </summary>
        public bool JobFolderExists(string jobNumber)
        {
            var jobPath = Path.Combine(_jobsBasePath, jobNumber);
            return Directory.Exists(jobPath);
        }

        /// <summary>
        /// Get component folder path for existing job
        /// </summary>
        public string GetComponentFolder(string jobNumber, string componentType, string variant = null)
        {
            var jobFolder = Path.Combine(_jobsBasePath, jobNumber);

            return componentType switch
            {
                "Bundle" => Path.Combine(jobFolder, "Bundle"),
                "Header" when !string.IsNullOrEmpty(variant) => Path.Combine(jobFolder, "Headers", variant),
                "Header" => Path.Combine(jobFolder, "Headers"),
                "XCH Structure" => Path.Combine(jobFolder, "Structure", "XCH"),
                "Z Structure" => Path.Combine(jobFolder, "Structure", "Z"),
                _ => Path.Combine(jobFolder, componentType)
            };
        }
    }

    public class JobFolderResult
    {
        public bool Success { get; set; }
        public string JobNumber { get; set; }
        public string JobFolderPath { get; set; }
        public string ComponentFolderPath { get; set; }
        public string Message { get; set; }
        public string Error { get; set; }
    }
}
