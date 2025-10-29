
using System.Collections.Generic;
using UnifiedUI.Models.Workflow;

namespace UnifiedUI.Services.Workflow
{
    /// <summary>
    /// Interface for managing workflow templates
    /// </summary>
    public interface IWorkflowTemplateService
    {
        /// <summary>
        /// Gets all available workflow templates
        /// </summary>
        /// <returns>List of workflow templates</returns>
        List<WorkflowDefinition> GetAllTemplates();

        /// <summary>
        /// Gets a template by name
        /// </summary>
        /// <param name="templateName">Template name</param>
        /// <returns>Template or null</returns>
        WorkflowDefinition GetTemplate(string templateName);

        /// <summary>
        /// Creates a workflow instance from a template
        /// </summary>
        /// <param name="templateName">Template name</param>
        /// <param name="workflowName">Name for the new workflow</param>
        /// <returns>New workflow instance</returns>
        WorkflowDefinition CreateFromTemplate(string templateName, string workflowName);

        /// <summary>
        /// Saves a workflow as a template
        /// </summary>
        /// <param name="workflow">Workflow to save as template</param>
        /// <param name="templateName">Template name</param>
        /// <returns>True if successful</returns>
        bool SaveAsTemplate(WorkflowDefinition workflow, string templateName);

        /// <summary>
        /// Deletes a template
        /// </summary>
        /// <param name="templateName">Template name</param>
        /// <returns>True if successful</returns>
        bool DeleteTemplate(string templateName);
    }
}
