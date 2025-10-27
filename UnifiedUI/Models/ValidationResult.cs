using System.Collections.Generic;

namespace UnifiedUI.Models
{
    /// <summary>
    /// Result of validation operation
    /// </summary>
    public class ValidationResult
    {
        public bool IsValid { get; set; }
        public int ValidCount { get; set; }
        public List<string> Errors { get; set; }
        public List<string> Warnings { get; set; }

        public ValidationResult()
        {
            Errors = new List<string>();
            Warnings = new List<string>();
            IsValid = true;
            ValidCount = 0;
        }

        public void AddError(string message)
        {
            Errors.Add(message);
            IsValid = false;
        }

        public void AddWarning(string message)
        {
            Warnings.Add(message);
        }
    }
}
