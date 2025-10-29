
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Newtonsoft.Json;
using UnifiedUI.Models.Workflow;
using FileTools.Infrastructure;

namespace UnifiedUI.Services.Workflow
{
    /// <summary>
    /// Implementation of workflow persistence using JSON files
    /// </summary>
    public class WorkflowPersistenceService : IWorkflowPersistenceService
    {
        private readonly string _workflowsFolder;
        private readonly JsonSerializerSettings _jsonSettings;

        public WorkflowPersistenceService(string workflowsFolder = null)
        {
            _workflowsFolder = workflowsFolder ?? Path.Combine(
                Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData),
                "SolidWorksAutomation",
                "Workflows"
            );

            // Ensure folder exists
            if (!Directory.Exists(_workflowsFolder))
            {
                Directory.CreateDirectory(_workflowsFolder);
            }

            _jsonSettings = new JsonSerializerSettings
            {
                Formatting = Formatting.Indented,
                TypeNameHandling = TypeNameHandling.Auto,
                NullValueHandling = NullValueHandling.Ignore
            };
        }

        public bool SaveWorkflow(WorkflowDefinition workflow)
        {
            if (workflow == null)
            {
                GlobalErrorHandler.LogError("Cannot save null workflow");
                return false;
            }

            try
            {
                workflow.ModifiedDate = DateTime.Now;
                
                var filePath = GetWorkflowFilePath(workflow.Id);
                var json = JsonConvert.SerializeObject(workflow, _jsonSettings);
                
                File.WriteAllText(filePath, json);
                
                GlobalErrorHandler.LogInfo($"Workflow saved: {workflow.Name} ({workflow.Id})");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error saving workflow: {workflow.Name}");
                return false;
            }
        }

        public WorkflowDefinition LoadWorkflow(string workflowId)
        {
            if (string.IsNullOrWhiteSpace(workflowId))
            {
                return null;
            }

            try
            {
                var filePath = GetWorkflowFilePath(workflowId);
                
                if (!File.Exists(filePath))
                {
                    GlobalErrorHandler.LogError($"Workflow file not found: {workflowId}");
                    return null;
                }

                var json = File.ReadAllText(filePath);
                var workflow = JsonConvert.DeserializeObject<WorkflowDefinition>(json, _jsonSettings);
                
                GlobalErrorHandler.LogInfo($"Workflow loaded: {workflow?.Name} ({workflowId})");
                return workflow;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error loading workflow: {workflowId}");
                return null;
            }
        }

        public List<WorkflowDefinition> LoadAllWorkflows()
        {
            var workflows = new List<WorkflowDefinition>();

            try
            {
                var files = Directory.GetFiles(_workflowsFolder, "*.json");
                
                foreach (var file in files)
                {
                    try
                    {
                        var json = File.ReadAllText(file);
                        var workflow = JsonConvert.DeserializeObject<WorkflowDefinition>(json, _jsonSettings);
                        
                        if (workflow != null)
                        {
                            workflows.Add(workflow);
                        }
                    }
                    catch (Exception ex)
                    {
                        GlobalErrorHandler.LogError(ex, $"Error loading workflow file: {file}");
                    }
                }

                GlobalErrorHandler.LogInfo($"Loaded {workflows.Count} workflows");
                return workflows.OrderByDescending(w => w.ModifiedDate).ToList();
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error loading workflows");
                return workflows;
            }
        }

        public bool DeleteWorkflow(string workflowId)
        {
            if (string.IsNullOrWhiteSpace(workflowId))
            {
                return false;
            }

            try
            {
                var filePath = GetWorkflowFilePath(workflowId);
                
                if (File.Exists(filePath))
                {
                    File.Delete(filePath);
                    GlobalErrorHandler.LogInfo($"Workflow deleted: {workflowId}");
                    return true;
                }
                
                return false;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error deleting workflow: {workflowId}");
                return false;
            }
        }

        public bool ExportWorkflow(WorkflowDefinition workflow, string filePath)
        {
            if (workflow == null || string.IsNullOrWhiteSpace(filePath))
            {
                return false;
            }

            try
            {
                var json = JsonConvert.SerializeObject(workflow, _jsonSettings);
                File.WriteAllText(filePath, json);
                
                GlobalErrorHandler.LogInfo($"Workflow exported: {workflow.Name} to {filePath}");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error exporting workflow: {workflow.Name}");
                return false;
            }
        }

        public WorkflowDefinition ImportWorkflow(string filePath)
        {
            if (string.IsNullOrWhiteSpace(filePath) || !File.Exists(filePath))
            {
                return null;
            }

            try
            {
                var json = File.ReadAllText(filePath);
                var workflow = JsonConvert.DeserializeObject<WorkflowDefinition>(json, _jsonSettings);
                
                if (workflow != null)
                {
                    // Generate new ID for imported workflow
                    workflow.Id = Guid.NewGuid().ToString();
                    workflow.CreatedDate = DateTime.Now;
                    workflow.ModifiedDate = DateTime.Now;
                    
                    // Save imported workflow
                    SaveWorkflow(workflow);
                    
                    GlobalErrorHandler.LogInfo($"Workflow imported: {workflow.Name}");
                }
                
                return workflow;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error importing workflow from: {filePath}");
                return null;
            }
        }

        private string GetWorkflowFilePath(string workflowId)
        {
            return Path.Combine(_workflowsFolder, $"{workflowId}.json");
        }
    }
}
