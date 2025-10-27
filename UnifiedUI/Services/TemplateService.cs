using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Newtonsoft.Json;
using UnifiedUI.Models;

namespace UnifiedUI.Services
{
    /// <summary>
    /// Service for managing configuration templates
    /// </summary>
    public class TemplateService
    {
        private const string TemplatesFolder = "templates/configs";

        public List<Template> GetAvailableTemplates()
        {
            var templates = new List<Template>();

            // Add some default templates
            templates.Add(new Template
            {
                Name = "Standard 2-Row Bundle",
                Description = "Standard bundle with 2 rows of tubes",
                ComponentType = "Bundle",
                Configuration = new BundleConfiguration
                {
                    BundleWidth = 48.0,
                    TubeOD = 1.0,
                    TubeRow1Count = 24,
                    TubeRow2Count = 23
                }
            });

            templates.Add(new Template
            {
                Name = "Standard Header (61)",
                Description = "Standard header type 61",
                ComponentType = "Header",
                Configuration = new HeaderConfiguration
                {
                    HeaderType = "61",
                    BoxWidth = 24.0,
                    BoxHeight = 18.0
                }
            });

            // TODO: Load templates from disk
            // LoadTemplatesFromDisk(templates);

            return templates;
        }

        public ComponentConfiguration LoadTemplate(Template template)
        {
            if (template?.Configuration == null)
                return new ComponentConfiguration();

            // Clone the configuration
            var json = JsonConvert.SerializeObject(template.Configuration);
            var config = JsonConvert.DeserializeObject<ComponentConfiguration>(json);
            
            // Update metadata
            template.LastUsed = DateTime.Now;
            template.UsageCount++;
            
            return config;
        }

        public void SaveConfiguration(ComponentConfiguration config, string filePath)
        {
            var json = JsonConvert.SerializeObject(config, Formatting.Indented);
            
            var directory = Path.GetDirectoryName(filePath);
            if (!Directory.Exists(directory))
            {
                Directory.CreateDirectory(directory);
            }
            
            File.WriteAllText(filePath, json);
        }

        private void LoadTemplatesFromDisk(List<Template> templates)
        {
            if (!Directory.Exists(TemplatesFolder))
                return;

            var files = Directory.GetFiles(TemplatesFolder, "*.json");
            foreach (var file in files)
            {
                try
                {
                    var json = File.ReadAllText(file);
                    var template = JsonConvert.DeserializeObject<Template>(json);
                    if (template != null)
                    {
                        templates.Add(template);
                    }
                }
                catch
                {
                    // Skip invalid template files
                }
            }
        }
    }
}
