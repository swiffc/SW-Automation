
using System;
using System.Threading.Tasks;
using UnifiedUI.Models.Workflow;

namespace UnifiedUI.Services.Workflow
{
    /// <summary>
    /// Interface for workflow execution engine
    /// </summary>
    public interface IWorkflowEngine
    {
        /// <summary>
        /// Executes a workflow definition
        /// </summary>
        /// <param name="workflow">Workflow definition to execute</param>
        /// <param name="progressCallback">Optional progress callback</param>
        /// <returns>Execution context with results</returns>
        Task<WorkflowExecutionContext> ExecuteWorkflowAsync(
            WorkflowDefinition workflow,
            Action<WorkflowProgressEventArgs> progressCallback = null);

        /// <summary>
        /// Pauses the currently executing workflow
        /// </summary>
        /// <param name="executionId">Execution ID</param>
        /// <returns>True if paused successfully</returns>
        bool PauseWorkflow(string executionId);

        /// <summary>
        /// Resumes a paused workflow
        /// </summary>
        /// <param name="executionId">Execution ID</param>
        /// <returns>True if resumed successfully</returns>
        bool ResumeWorkflow(string executionId);

        /// <summary>
        /// Cancels a running workflow
        /// </summary>
        /// <param name="executionId">Execution ID</param>
        /// <returns>True if cancelled successfully</returns>
        bool CancelWorkflow(string executionId);

        /// <summary>
        /// Gets the current execution context
        /// </summary>
        /// <param name="executionId">Execution ID</param>
        /// <returns>Execution context or null</returns>
        WorkflowExecutionContext GetExecutionContext(string executionId);
    }
}
