
using System;
using System.Collections.Generic;

namespace UnifiedUI.Models.Workflow
{
    /// <summary>
    /// Represents a single step in a workflow
    /// </summary>
    public class WorkflowStep
    {
        public string Id { get; set; }
        public string Name { get; set; }
        public string Description { get; set; }
        public WorkflowStepType StepType { get; set; }
        public string ActionName { get; set; }
        public Dictionary<string, object> Parameters { get; set; }
        public int Order { get; set; }
        public bool IsEnabled { get; set; }
        public bool ContinueOnError { get; set; }
        public int RetryCount { get; set; }
        public int TimeoutSeconds { get; set; }
        public string SuccessCondition { get; set; }
        public List<string> DependsOn { get; set; }

        public WorkflowStep()
        {
            Id = Guid.NewGuid().ToString();
            Parameters = new Dictionary<string, object>();
            IsEnabled = true;
            ContinueOnError = false;
            RetryCount = 0;
            TimeoutSeconds = 300; // 5 minutes default
            DependsOn = new List<string>();
        }

        /// <summary>
        /// Validates the workflow step
        /// </summary>
        public ValidationResult Validate()
        {
            var result = new ValidationResult { IsValid = true };

            if (string.IsNullOrWhiteSpace(Name))
            {
                result.IsValid = false;
                result.Errors.Add("Step name is required");
            }

            if (string.IsNullOrWhiteSpace(ActionName))
            {
                result.IsValid = false;
                result.Errors.Add("Action name is required");
            }

            if (TimeoutSeconds <= 0)
            {
                result.IsValid = false;
                result.Errors.Add("Timeout must be greater than 0");
            }

            return result;
        }
    }

    /// <summary>
    /// Types of workflow steps
    /// </summary>
    public enum WorkflowStepType
    {
        OpenDocument,
        SaveDocument,
        CloseDocument,
        CreateFeature,
        ModifyFeature,
        SetProperty,
        ExportDocument,
        CreateDrawing,
        CreateAssembly,
        InsertComponent,
        RunScript,
        WaitForCompletion,
        Conditional,
        Loop,
        Custom
    }
}
