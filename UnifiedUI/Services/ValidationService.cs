using System;
using UnifiedUI.Models;

namespace UnifiedUI.Services
{
    /// <summary>
    /// Service for validating component configurations
    /// </summary>
    public class ValidationService
    {
        public ValidationResult Validate(ComponentConfiguration config)
        {
            var result = new ValidationResult();

            if (config == null)
            {
                result.AddError("Configuration is null");
                return result;
            }

            // Validate common fields
            ValidateJobNumber(config, result);
            ValidateDimensions(config, result);
            ValidateComponentSpecific(config, result);

            return result;
        }

        private void ValidateJobNumber(ComponentConfiguration config, ValidationResult result)
        {
            if (string.IsNullOrWhiteSpace(config.JobNumber))
            {
                result.AddError("Job Number is required");
            }
            else if (!config.JobNumber.StartsWith("S2"))
            {
                result.AddWarning("Job Number should start with 'S2'");
            }
            else
            {
                result.ValidCount++;
            }
        }

        private void ValidateDimensions(ComponentConfiguration config, ValidationResult result)
        {
            // For Bundle, use BundleWidth instead of base Width
            if (config is BundleConfiguration bundleConfig)
            {
                // Validate BundleWidth
                if (bundleConfig.BundleWidth <= 0)
                {
                    result.AddError("Width must be greater than 0");
                }
                else if (bundleConfig.BundleWidth < 24 || bundleConfig.BundleWidth > 120)
                {
                    result.AddWarning($"Bundle Width {bundleConfig.BundleWidth}\" is outside typical range (24-120 inches)");
                    result.ValidCount++;
                }
                else
                {
                    result.ValidCount++;
                }

                // Validate BundleDepth
                if (bundleConfig.BundleDepth <= 0)
                {
                    // Don't error if not set yet, just skip
                    result.ValidCount++; // Count as valid if user hasn't entered it yet
                }
                else
                {
                    result.ValidCount++;
                }

                // Height for Bundle is calculated, so skip validation
                result.ValidCount++; // Auto-pass height for Bundle
                return;
            }

            // For other components, use base properties
            // Width validation
            if (config.Width <= 0)
            {
                // Don't error - might not be set yet
                result.ValidCount++;
            }
            else if (config.Width < 24 || config.Width > 96)
            {
                result.AddWarning($"Width {config.Width}\" is outside typical range (24-96 inches)");
                result.ValidCount++;
            }
            else
            {
                result.ValidCount++;
            }

            // Height validation
            if (config.Height <= 0)
            {
                // Don't error - might not be set yet
                result.ValidCount++;
            }
            else
            {
                result.ValidCount++;
            }

            // Depth validation
            if (config.Depth <= 0)
            {
                // Don't error - might not be set yet
                result.ValidCount++;
            }
            else
            {
                result.ValidCount++;
            }
        }

        private void ValidateComponentSpecific(ComponentConfiguration config, ValidationResult result)
        {
            switch (config.ComponentType)
            {
                case "Bundle":
                    ValidateBundle(config as BundleConfiguration, result);
                    break;
                case "Header":
                    ValidateHeader(config as HeaderConfiguration, result);
                    break;
                // Add other component types as needed
            }
        }

        private void ValidateBundle(BundleConfiguration config, ValidationResult result)
        {
            if (config == null) return;

            // TubeOD validation - make lenient, allow 0 if not set yet
            if (config.TubeOD < 0)
            {
                result.AddError("Tube OD cannot be negative");
            }
            else if (config.TubeOD == 0)
            {
                // Not entered yet, don't error
                result.ValidCount++;
            }
            else if (config.TubeOD < 0.5 || config.TubeOD > 2.0)
            {
                result.AddWarning($"Tube OD {config.TubeOD}\" is outside typical range (0.5-2.0 inches)");
                result.ValidCount++;
            }
            else
            {
                result.ValidCount++;
            }

            // Tube row count validation - make lenient
            if (config.TubeRow1Count == 0 && config.TubeRow2Count == 0)
            {
                // Not entered yet, don't error
                result.ValidCount++;
            }
            else if (config.TubeRow1Count < 0 || config.TubeRow2Count < 0)
            {
                result.AddError("Tube row counts cannot be negative");
            }
            else
            {
                result.ValidCount++;
            }
        }

        private void ValidateHeader(HeaderConfiguration config, ValidationResult result)
        {
            if (config == null) return;

            if (config.BoxWidth <= 0)
            {
                result.AddError("Box Width must be greater than 0");
            }
            else
            {
                result.ValidCount++;
            }

            if (config.BoxHeight <= 0)
            {
                result.AddError("Box Height must be greater than 0");
            }
            else
            {
                result.ValidCount++;
            }
        }
    }
}
