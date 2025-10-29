
using System;

namespace UnifiedUI.Models.Workflow
{
    /// <summary>
    /// Result of a workflow step execution
    /// </summary>
    public class WorkflowStepResult
    {
        public string StepId { get; set; }
        public string StepName { get; set; }
        public DateTime StartTime { get; set; }
        public DateTime EndTime { get; set; }
        public TimeSpan Duration => EndTime - StartTime;
        public bool Success { get; set; }
        public string Message { get; set; }
        public string ErrorMessage { get; set; }
        public Exception Exception { get; set; }
        public object Output { get; set; }
        public int RetryAttempt { get; set; }

        public WorkflowStepResult()
        {
            StartTime = DateTime.Now;
        }

        /// <summary>
        /// Marks the step as completed successfully
        /// </summary>
        public void MarkSuccess(string message = "", object output = null)
        {
            EndTime = DateTime.Now;
            Success = true;
            Message = message;
            Output = output;
        }

        /// <summary>
        /// Marks the step as failed
        /// </summary>
        public void MarkFailure(string errorMessage, Exception exception = null)
        {
            EndTime = DateTime.Now;
            Success = false;
            ErrorMessage = errorMessage;
            Exception = exception;
        }
    }
}
