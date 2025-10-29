
using System;
using System.Collections.Generic;
using System.Linq;
using UnifiedUI.Models.Workflow;
using FileTools.Infrastructure;

namespace UnifiedUI.Services.Workflow
{
    /// <summary>
    /// Implementation of workflow template service
    /// </summary>
    public class WorkflowTemplateService : IWorkflowTemplateService
    {
        private readonly IWorkflowPersistenceService _persistenceService;
        private readonly Dictionary<string, WorkflowDefinition> _builtInTemplates;

        public WorkflowTemplateService(IWorkflowPersistenceService persistenceService)
        {
            _persistenceService = persistenceService ?? throw new ArgumentNullException(nameof(persistenceService));
            _builtInTemplates = new Dictionary<string, WorkflowDefinition>();
            InitializeBuiltInTemplates();
        }

        public List<WorkflowDefinition> GetAllTemplates()
        {
            var templates = new List<WorkflowDefinition>();
            
            // Add built-in templates
            templates.AddRange(_builtInTemplates.Values);
            
            // Add user templates
            var allWorkflows = _persistenceService.LoadAllWorkflows();
            var userTemplates = allWorkflows.Where(w => w.IsTemplate).ToList();
            templates.AddRange(userTemplates);

            GlobalErrorHandler.LogInfo($"Found {templates.Count} workflow templates");
            return templates;
        }

        public WorkflowDefinition GetTemplate(string templateName)
        {
            // Check built-in templates first
            if (_builtInTemplates.TryGetValue(templateName, out var builtInTemplate))
            {
                return builtInTemplate;
            }

            // Check user templates
            var allWorkflows = _persistenceService.LoadAllWorkflows();
            var userTemplate = allWorkflows.FirstOrDefault(w => 
                w.IsTemplate && w.TemplateName.Equals(templateName, StringComparison.OrdinalIgnoreCase));

            return userTemplate;
        }

        public WorkflowDefinition CreateFromTemplate(string templateName, string workflowName)
        {
            var template = GetTemplate(templateName);
            
            if (template == null)
            {
                GlobalErrorHandler.LogError($"Template not found: {templateName}");
                return null;
            }

            try
            {
                // Deep copy the template
                var json = Newtonsoft.Json.JsonConvert.SerializeObject(template);
                var newWorkflow = Newtonsoft.Json.JsonConvert.DeserializeObject<WorkflowDefinition>(json);

                // Update workflow properties
                newWorkflow.Id = Guid.NewGuid().ToString();
                newWorkflow.Name = workflowName;
                newWorkflow.CreatedDate = DateTime.Now;
                newWorkflow.ModifiedDate = DateTime.Now;
                newWorkflow.IsTemplate = false;
                newWorkflow.TemplateName = templateName;

                // Generate new IDs for all steps
                foreach (var step in newWorkflow.Steps)
                {
                    step.Id = Guid.NewGuid().ToString();
                }

                GlobalErrorHandler.LogInfo($"Created workflow from template: {templateName} -> {workflowName}");
                return newWorkflow;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error creating workflow from template: {templateName}");
                return null;
            }
        }

        public bool SaveAsTemplate(WorkflowDefinition workflow, string templateName)
        {
            if (workflow == null || string.IsNullOrWhiteSpace(templateName))
            {
                return false;
            }

            try
            {
                // Deep copy the workflow
                var json = Newtonsoft.Json.JsonConvert.SerializeObject(workflow);
                var template = Newtonsoft.Json.JsonConvert.DeserializeObject<WorkflowDefinition>(json);

                // Update template properties
                template.Id = Guid.NewGuid().ToString();
                template.Name = $"{templateName} (Template)";
                template.IsTemplate = true;
                template.TemplateName = templateName;
                template.CreatedDate = DateTime.Now;
                template.ModifiedDate = DateTime.Now;

                // Save template
                bool success = _persistenceService.SaveWorkflow(template);
                
                if (success)
                {
                    GlobalErrorHandler.LogInfo($"Saved workflow as template: {templateName}");
                }
                
                return success;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error saving workflow as template: {templateName}");
                return false;
            }
        }

        public bool DeleteTemplate(string templateName)
        {
            // Cannot delete built-in templates
            if (_builtInTemplates.ContainsKey(templateName))
            {
                GlobalErrorHandler.LogError($"Cannot delete built-in template: {templateName}");
                return false;
            }

            // Find and delete user template
            var allWorkflows = _persistenceService.LoadAllWorkflows();
            var template = allWorkflows.FirstOrDefault(w => 
                w.IsTemplate && w.TemplateName.Equals(templateName, StringComparison.OrdinalIgnoreCase));

            if (template != null)
            {
                return _persistenceService.DeleteWorkflow(template.Id);
            }

            return false;
        }

        private void InitializeBuiltInTemplates()
        {
            // Built-in template: Simple Document Processing
            var simpleDocTemplate = new WorkflowDefinition
            {
                Name = "Simple Document Processing",
                Description = "Opens a document, modifies properties, and saves it",
                Category = "Basic",
                IsTemplate = true,
                TemplateName = "SimpleDocumentProcessing",
                ExecutionMode = WorkflowExecutionMode.Sequential,
                Steps = new List<WorkflowStep>
                {
                    new WorkflowStep
                    {
                        Name = "Open Document",
                        Description = "Opens the SolidWorks document",
                        StepType = WorkflowStepType.OpenDocument,
                        ActionName = "OpenDocument",
                        Order = 1,
                        Parameters = new Dictionary<string, object>
                        {
                            { "FilePath", "{DocumentPath}" }
                        }
                    },
                    new WorkflowStep
                    {
                        Name = "Set Custom Properties",
                        Description = "Sets custom properties on the document",
                        StepType = WorkflowStepType.SetProperty,
                        ActionName = "SetProperties",
                        Order = 2,
                        Parameters = new Dictionary<string, object>
                        {
                            { "PropertyName", "{PropertyName}" },
                            { "PropertyValue", "{PropertyValue}" }
                        }
                    },
                    new WorkflowStep
                    {
                        Name = "Save Document",
                        Description = "Saves the modified document",
                        StepType = WorkflowStepType.SaveDocument,
                        ActionName = "SaveDocument",
                        Order = 3
                    },
                    new WorkflowStep
                    {
                        Name = "Close Document",
                        Description = "Closes the document",
                        StepType = WorkflowStepType.CloseDocument,
                        ActionName = "CloseDocument",
                        Order = 4
                    }
                }
            };

            _builtInTemplates["SimpleDocumentProcessing"] = simpleDocTemplate;

            // Built-in template: Batch Export
            var batchExportTemplate = new WorkflowDefinition
            {
                Name = "Batch Export to PDF",
                Description = "Opens documents and exports them to PDF",
                Category = "Export",
                IsTemplate = true,
                TemplateName = "BatchExportPDF",
                ExecutionMode = WorkflowExecutionMode.Sequential,
                Steps = new List<WorkflowStep>
                {
                    new WorkflowStep
                    {
                        Name = "Open Document",
                        Description = "Opens the SolidWorks document",
                        StepType = WorkflowStepType.OpenDocument,
                        ActionName = "OpenDocument",
                        Order = 1,
                        Parameters = new Dictionary<string, object>
                        {
                            { "FilePath", "{DocumentPath}" }
                        }
                    },
                    new WorkflowStep
                    {
                        Name = "Export to PDF",
                        Description = "Exports document to PDF format",
                        StepType = WorkflowStepType.ExportDocument,
                        ActionName = "ExportDocument",
                        Order = 2,
                        Parameters = new Dictionary<string, object>
                        {
                            { "FilePath", "{OutputPath}" },
                            { "Format", "PDF" }
                        }
                    },
                    new WorkflowStep
                    {
                        Name = "Close Document",
                        Description = "Closes the document",
                        StepType = WorkflowStepType.CloseDocument,
                        ActionName = "CloseDocument",
                        Order = 3
                    }
                }
            };

            _builtInTemplates["BatchExportPDF"] = batchExportTemplate;

            GlobalErrorHandler.LogInfo($"Initialized {_builtInTemplates.Count} built-in workflow templates");
        }
    }
}
