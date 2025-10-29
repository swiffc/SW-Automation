
using System;
using System.Collections.Generic;

namespace UnifiedUI.Models.Workflow
{
    /// <summary>
    /// Defines a complete workflow with multiple steps
    /// </summary>
    public class WorkflowDefinition
    {
        public string Id { get; set; }
        public string Name { get; set; }
        public string Description { get; set; }
        public string Category { get; set; }
        public DateTime CreatedDate { get; set; }
        public DateTime ModifiedDate { get; set; }
        public string CreatedBy { get; set; }
        public int Version { get; set; }
        public List<WorkflowStep> Steps { get; set; }
        public Dictionary<string, object> GlobalParameters { get; set; }
        public WorkflowExecutionMode ExecutionMode { get; set; }
        public bool IsTemplate { get; set; }
        public string TemplateName { get; set; }

        public WorkflowDefinition()
        {
            Id = Guid.NewGuid().ToString();
            Steps = new List<WorkflowStep>();
            GlobalParameters = new Dictionary<string, object>();
            CreatedDate = DateTime.Now;
            ModifiedDate = DateTime.Now;
            Version = 1;
            ExecutionMode = WorkflowExecutionMode.Sequential;
        }

        /// <summary>
        /// Validates the workflow definition
        /// </summary>
        public ValidationResult Validate()
        {
            var result = new ValidationResult { IsValid = true };

            if (string.IsNullOrWhiteSpace(Name))
            {
                result.IsValid = false;
                result.Errors.Add("Workflow name is required");
            }

            if (Steps == null || Steps.Count == 0)
            {
                result.IsValid = false;
                result.Errors.Add("Workflow must have at least one step");
            }

            // Validate each step
            if (Steps != null)
            {
                for (int i = 0; i < Steps.Count; i++)
                {
                    var stepValidation = Steps[i].Validate();
                    if (!stepValidation.IsValid)
                    {
                        result.IsValid = false;
                        result.Errors.Add($"Step {i + 1}: {string.Join(", ", stepValidation.Errors)}");
                    }
                }
            }

            return result;
        }
    }

    /// <summary>
    /// Workflow execution mode
    /// </summary>
    public enum WorkflowExecutionMode
    {
        Sequential,
        Parallel,
        Conditional
    }
}
