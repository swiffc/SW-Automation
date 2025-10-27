using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace UnifiedUI.Services
{
    /// <summary>
    /// Manages copying and renaming template files for design table approach
    /// </summary>
    public class TemplateFileManager
    {
        private readonly string _templatesBasePath;

        public TemplateFileManager(string templatesBasePath = null)
        {
            _templatesBasePath = templatesBasePath ?? 
                Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\templates");
        }

        /// <summary>
        /// Copy template folder and rename all files with job number
        /// </summary>
        public TemplateResult CopyAndRenameTemplates(TemplateCopyRequest request)
        {
            var result = new TemplateResult { Success = true };

            try
            {
                // Validate source template exists
                if (!Directory.Exists(request.SourceTemplatePath))
                {
                    result.Success = false;
                    result.Error = $"Template not found: {request.SourceTemplatePath}";
                    return result;
                }

                // Create destination folder
                Directory.CreateDirectory(request.DestinationPath);

                // Get all files from template
                var templateFiles = Directory.GetFiles(request.SourceTemplatePath, "*.*", SearchOption.AllDirectories);

                foreach (var sourceFile in templateFiles)
                {
                    // Calculate relative path (manual implementation for .NET Framework)
                    var relativePath = sourceFile.Replace(request.SourceTemplatePath, "").TrimStart('\\', '/');
                    var destFile = Path.Combine(request.DestinationPath, relativePath);

                    // Create subdirectories if needed
                    var destDir = Path.GetDirectoryName(destFile);
                    if (!Directory.Exists(destDir))
                    {
                        Directory.CreateDirectory(destDir);
                    }

                    // Copy and rename file
                    var renamedFile = RenameFileWithJobNumber(destFile, request.OldPrefix, request.NewPrefix);
                    File.Copy(sourceFile, renamedFile, overwrite: true);

                    result.CopiedFiles.Add(new FileMapping
                    {
                        SourcePath = sourceFile,
                        DestinationPath = renamedFile,
                        FileName = Path.GetFileName(renamedFile)
                    });
                }

                result.Message = $"Successfully copied {result.CopiedFiles.Count} files";
            }
            catch (Exception ex)
            {
                result.Success = false;
                result.Error = $"Error copying templates: {ex.Message}";
            }

            return result;
        }

        /// <summary>
        /// Rename file from template prefix to job prefix
        /// </summary>
        private string RenameFileWithJobNumber(string filePath, string oldPrefix, string newPrefix)
        {
            var fileName = Path.GetFileName(filePath);
            var directory = Path.GetDirectoryName(filePath);

            // Replace old prefix with new prefix in filename
            var newFileName = fileName.Replace(oldPrefix, newPrefix);

            return Path.Combine(directory, newFileName);
        }

        /// <summary>
        /// Get template path for specific component type
        /// </summary>
        public string GetTemplatePath(string componentType, string variant = null)
        {
            // TODO: Update these paths to match your actual template locations
            return componentType switch
            {
                "Header" when variant == "S01c" => 
                    Path.Combine(_templatesBasePath, "header_section_tool", "Combined_"),
                
                "Header" when variant == "S03" => 
                    Path.Combine(_templatesBasePath, "header_section_tool", "Single_"),
                
                "XCH Structure" => 
                    Path.Combine(_templatesBasePath, "xch_structure_tool"),
                
                "Z Structure" => 
                    Path.Combine(_templatesBasePath, "z_structure_tool"),
                
                _ => null
            };
        }

        /// <summary>
        /// Get list of key files to update (Excel configuration files)
        /// </summary>
        public List<string> GetExcelFilesToUpdate(string destinationPath, string componentType)
        {
            var excelFiles = new List<string>();

            // Find all Excel files in destination
            if (Directory.Exists(destinationPath))
            {
                excelFiles.AddRange(Directory.GetFiles(destinationPath, "*HCS.xlsx", SearchOption.AllDirectories));
                excelFiles.AddRange(Directory.GetFiles(destinationPath, "*SCS.xlsx", SearchOption.AllDirectories));
                excelFiles.AddRange(Directory.GetFiles(destinationPath, "*SFCS.xlsx", SearchOption.AllDirectories));
            }

            return excelFiles;
        }
    }

    #region Supporting Classes

    public class TemplateCopyRequest
    {
        public string SourceTemplatePath { get; set; }
        public string DestinationPath { get; set; }
        public string OldPrefix { get; set; } // e.g., "000000"
        public string NewPrefix { get; set; } // e.g., "S2XXXX"
    }

    public class TemplateResult
    {
        public bool Success { get; set; }
        public string Message { get; set; }
        public string Error { get; set; }
        public List<FileMapping> CopiedFiles { get; set; }

        public TemplateResult()
        {
            CopiedFiles = new List<FileMapping>();
        }
    }

    public class FileMapping
    {
        public string SourcePath { get; set; }
        public string DestinationPath { get; set; }
        public string FileName { get; set; }
    }

    #endregion
}
