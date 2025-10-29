
using System;
using System.Collections.Generic;

namespace UnifiedUI.Models.Workflow
{
    /// <summary>
    /// Context for workflow execution containing runtime state
    /// </summary>
    public class WorkflowExecutionContext
    {
        public string WorkflowId { get; set; }
        public string ExecutionId { get; set; }
        public DateTime StartTime { get; set; }
        public DateTime? EndTime { get; set; }
        public WorkflowExecutionStatus Status { get; set; }
        public string CurrentStepId { get; set; }
        public Dictionary<string, object> Variables { get; set; }
        public List<WorkflowStepResult> StepResults { get; set; }
        public string ErrorMessage { get; set; }
        public Exception LastException { get; set; }
        public int TotalSteps { get; set; }
        public int CompletedSteps { get; set; }
        public Action<WorkflowProgressEventArgs> ProgressCallback { get; set; }

        public WorkflowExecutionContext()
        {
            ExecutionId = Guid.NewGuid().ToString();
            StartTime = DateTime.Now;
            Status = WorkflowExecutionStatus.NotStarted;
            Variables = new Dictionary<string, object>();
            StepResults = new List<WorkflowStepResult>();
        }

        /// <summary>
        /// Gets the progress percentage (0-100)
        /// </summary>
        public int ProgressPercentage
        {
            get
            {
                if (TotalSteps == 0) return 0;
                return (int)((double)CompletedSteps / TotalSteps * 100);
            }
        }

        /// <summary>
        /// Reports progress to the callback
        /// </summary>
        public void ReportProgress(string message, int? progressOverride = null)
        {
            ProgressCallback?.Invoke(new WorkflowProgressEventArgs
            {
                WorkflowId = WorkflowId,
                ExecutionId = ExecutionId,
                CurrentStep = CurrentStepId,
                Message = message,
                Progress = progressOverride ?? ProgressPercentage,
                Status = Status
            });
        }
    }

    /// <summary>
    /// Workflow execution status
    /// </summary>
    public enum WorkflowExecutionStatus
    {
        NotStarted,
        Running,
        Paused,
        Completed,
        Failed,
        Cancelled
    }

    /// <summary>
    /// Progress event arguments
    /// </summary>
    public class WorkflowProgressEventArgs : EventArgs
    {
        public string WorkflowId { get; set; }
        public string ExecutionId { get; set; }
        public string CurrentStep { get; set; }
        public string Message { get; set; }
        public int Progress { get; set; }
        public WorkflowExecutionStatus Status { get; set; }
    }
}
