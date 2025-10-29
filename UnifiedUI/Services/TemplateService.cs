using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using UnifiedUI.Models;
using FileTools.Infrastructure;

namespace UnifiedUI.Services
{
    /// <summary>
    /// Service for loading and managing SolidWorks template files
    /// </summary>
    public class TemplateService
    {
        private readonly string _projectRoot;
        private Dictionary<ToolType, List<Template>> _templateCache;

        public TemplateService()
        {
            // Determine project root (assumes we're in output/bin folder)
            _projectRoot = GetProjectRoot();
            _templateCache = new Dictionary<ToolType, List<Template>>();
            
            GlobalErrorHandler.LogInfo($"TemplateService initialized. Project root: {_projectRoot}");
        }

        /// <summary>
        /// Get project root directory (where templates/ folder is located)
        /// </summary>
        private string GetProjectRoot()
        {
            var currentDir = AppDomain.CurrentDomain.BaseDirectory;
            
            // Try to find templates folder by walking up the directory tree
            var dir = new DirectoryInfo(currentDir);
            while (dir != null)
            {
                var templatesPath = Path.Combine(dir.FullName, "templates");
                if (Directory.Exists(templatesPath))
                {
                    return dir.FullName;
                }
                dir = dir.Parent;
            }
            
            // Fallback: assume standard development structure
            GlobalErrorHandler.LogWarning("Could not auto-detect project root. Using default.");
            return @"C:\Users\DCornealius\CascadeProjects\Solidworks_Automation";
        }

        /// <summary>
        /// Load all templates for a specific tool
        /// </summary>
        public List<Template> LoadTemplatesForTool(ToolType tool)
        {
            if (tool == null)
            {
                GlobalErrorHandler.LogWarning("ToolType is null");
                return new List<Template>();
            }

            // Return cached templates if already loaded
            if (_templateCache.ContainsKey(tool))
            {
                GlobalErrorHandler.LogInfo($"Returning {_templateCache[tool].Count} cached templates for {tool.Name}");
                return _templateCache[tool];
            }

            try
            {
                var templates = new List<Template>();
                var templateDir = Path.Combine(_projectRoot, tool.TemplatePath);

                GlobalErrorHandler.LogInfo($"Scanning template directory: {templateDir}");

                if (!Directory.Exists(templateDir))
                {
                    GlobalErrorHandler.LogWarning($"Template directory not found: {templateDir}");
                    return templates;
                }

                // Find all .SLDASM files recursively
                var assemblyFiles = Directory.GetFiles(templateDir, "*.SLDASM", SearchOption.AllDirectories);
                
                GlobalErrorHandler.LogInfo($"Found {assemblyFiles.Length} assembly files");

                foreach (var filePath in assemblyFiles)
                {
                    var template = new Template(filePath, tool);
                    templates.Add(template);
                }

                // Sort templates by display name
                templates = templates.OrderBy(t => t.Category)
                                   .ThenBy(t => t.DisplayName)
                                   .ToList();

                // Cache the results
                _templateCache[tool] = templates;

                GlobalErrorHandler.LogInfo($"? Loaded {templates.Count} templates for {tool.Name}");
                return templates;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"LoadTemplatesForTool({tool?.Name})");
                return new List<Template>();
            }
        }

        /// <summary>
        /// Get template by file name
        /// </summary>
        public Template GetTemplateByName(string fileName, ToolType tool)
        {
            var templates = LoadTemplatesForTool(tool);
            return templates.FirstOrDefault(t => 
                t.FileName.Equals(fileName, StringComparison.OrdinalIgnoreCase));
        }

        /// <summary>
        /// Get templates by category (e.g., "Single", "Combined", "Job-Specific")
        /// </summary>
        public List<Template> GetTemplatesByCategory(string category, ToolType tool)
        {
            var templates = LoadTemplatesForTool(tool);
            return templates.Where(t => 
                t.Category.Equals(category, StringComparison.OrdinalIgnoreCase))
                .ToList();
        }

        /// <summary>
        /// Search templates by display name
        /// </summary>
        public List<Template> SearchTemplates(string searchTerm, ToolType tool)
        {
            if (string.IsNullOrWhiteSpace(searchTerm))
                return LoadTemplatesForTool(tool);

            var templates = LoadTemplatesForTool(tool);
            return templates.Where(t => 
                t.DisplayName.IndexOf(searchTerm, StringComparison.OrdinalIgnoreCase) >= 0)
                .ToList();
        }

        /// <summary>
        /// Clear template cache (force reload on next access)
        /// </summary>
        public void ClearCache()
        {
            _templateCache.Clear();
            GlobalErrorHandler.LogInfo("Template cache cleared");
        }

        /// <summary>
        /// Clear cache for specific tool
        /// </summary>
        public void ClearCache(ToolType tool)
        {
            if (tool != null && _templateCache.ContainsKey(tool))
            {
                _templateCache.Remove(tool);
                GlobalErrorHandler.LogInfo($"Template cache cleared for {tool.Name}");
            }
        }

        /// <summary>
        /// Validate that a template file exists and is accessible
        /// </summary>
        public bool ValidateTemplate(Template template)
        {
            if (template == null || string.IsNullOrEmpty(template.FilePath))
                return false;

            if (!File.Exists(template.FilePath))
            {
                GlobalErrorHandler.LogWarning($"Template file not found: {template.FilePath}");
                return false;
            }

            try
            {
                // Try to access the file to ensure it's not locked
                using (var fs = File.OpenRead(template.FilePath))
                {
                    return true;
                }
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Template validation failed: {template.FileName}");
                return false;
            }
        }

        /// <summary>
        /// Get template statistics for a tool
        /// </summary>
        public TemplateStats GetTemplateStats(ToolType tool)
        {
            var templates = LoadTemplatesForTool(tool);
            return new TemplateStats
            {
                TotalCount = templates.Count,
                Categories = templates.GroupBy(t => t.Category)
                                    .ToDictionary(g => g.Key, g => g.Count()),
                ValidCount = templates.Count(t => t.IsValid),
                InvalidCount = templates.Count(t => !t.IsValid)
            };
        }
    }

    /// <summary>
    /// Statistics about templates for a tool
    /// </summary>
    public class TemplateStats
    {
        public int TotalCount { get; set; }
        public Dictionary<string, int> Categories { get; set; }
        public int ValidCount { get; set; }
        public int InvalidCount { get; set; }

        public override string ToString()
        {
            var categoryStr = string.Join(", ", Categories.Select(kvp => $"{kvp.Key}: {kvp.Value}"));
            return $"Total: {TotalCount} | Valid: {ValidCount} | Categories: {categoryStr}";
        }
    }
}
