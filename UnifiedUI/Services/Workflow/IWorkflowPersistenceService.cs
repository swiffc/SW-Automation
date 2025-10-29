
using System.Collections.Generic;
using UnifiedUI.Models.Workflow;

namespace UnifiedUI.Services.Workflow
{
    /// <summary>
    /// Interface for persisting workflows to storage
    /// </summary>
    public interface IWorkflowPersistenceService
    {
        /// <summary>
        /// Saves a workflow definition
        /// </summary>
        /// <param name="workflow">Workflow to save</param>
        /// <returns>True if successful</returns>
        bool SaveWorkflow(WorkflowDefinition workflow);

        /// <summary>
        /// Loads a workflow by ID
        /// </summary>
        /// <param name="workflowId">Workflow ID</param>
        /// <returns>Workflow definition or null</returns>
        WorkflowDefinition LoadWorkflow(string workflowId);

        /// <summary>
        /// Loads all workflows
        /// </summary>
        /// <returns>List of workflows</returns>
        List<WorkflowDefinition> LoadAllWorkflows();

        /// <summary>
        /// Deletes a workflow
        /// </summary>
        /// <param name="workflowId">Workflow ID</param>
        /// <returns>True if successful</returns>
        bool DeleteWorkflow(string workflowId);

        /// <summary>
        /// Exports workflow to file
        /// </summary>
        /// <param name="workflow">Workflow to export</param>
        /// <param name="filePath">File path</param>
        /// <returns>True if successful</returns>
        bool ExportWorkflow(WorkflowDefinition workflow, string filePath);

        /// <summary>
        /// Imports workflow from file
        /// </summary>
        /// <param name="filePath">File path</param>
        /// <returns>Imported workflow or null</returns>
        WorkflowDefinition ImportWorkflow(string filePath);
    }
}
