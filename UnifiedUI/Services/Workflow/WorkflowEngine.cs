
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using UnifiedUI.Models.Workflow;
using UnifiedUI.Services.SolidWorksApi;
using FileTools.Infrastructure;

namespace UnifiedUI.Services.Workflow
{
    /// <summary>
    /// Implementation of workflow execution engine
    /// </summary>
    public class WorkflowEngine : IWorkflowEngine
    {
        private readonly ISolidWorksConnectionManager _connectionManager;
        private readonly ISolidWorksDocumentService _documentService;
        private readonly ISolidWorksFeatureService _featureService;
        private readonly ISolidWorksPropertyService _propertyService;
        private readonly ISolidWorksDrawingService _drawingService;
        private readonly ISolidWorksAssemblyService _assemblyService;
        private readonly Dictionary<string, WorkflowExecutionContext> _activeExecutions;
        private readonly Dictionary<string, CancellationTokenSource> _cancellationTokens;

        public WorkflowEngine(
            ISolidWorksConnectionManager connectionManager,
            ISolidWorksDocumentService documentService,
            ISolidWorksFeatureService featureService,
            ISolidWorksPropertyService propertyService,
            ISolidWorksDrawingService drawingService,
            ISolidWorksAssemblyService assemblyService)
        {
            _connectionManager = connectionManager ?? throw new ArgumentNullException(nameof(connectionManager));
            _documentService = documentService ?? throw new ArgumentNullException(nameof(documentService));
            _featureService = featureService ?? throw new ArgumentNullException(nameof(featureService));
            _propertyService = propertyService ?? throw new ArgumentNullException(nameof(propertyService));
            _drawingService = drawingService ?? throw new ArgumentNullException(nameof(drawingService));
            _assemblyService = assemblyService ?? throw new ArgumentNullException(nameof(assemblyService));
            _activeExecutions = new Dictionary<string, WorkflowExecutionContext>();
            _cancellationTokens = new Dictionary<string, CancellationTokenSource>();
        }

        public async Task<WorkflowExecutionContext> ExecuteWorkflowAsync(
            WorkflowDefinition workflow,
            Action<WorkflowProgressEventArgs> progressCallback = null)
        {
            // Validate workflow
            var validation = workflow.Validate();
            if (!validation.IsValid)
            {
                var errorMsg = $"Workflow validation failed: {string.Join(", ", validation.Errors)}";
                GlobalErrorHandler.LogError(errorMsg);
                throw new InvalidOperationException(errorMsg);
            }

            // Create execution context
            var context = new WorkflowExecutionContext
            {
                WorkflowId = workflow.Id,
                Status = WorkflowExecutionStatus.Running,
                TotalSteps = workflow.Steps.Count,
                ProgressCallback = progressCallback
            };

            // Store execution context
            _activeExecutions[context.ExecutionId] = context;
            
            // Create cancellation token
            var cts = new CancellationTokenSource();
            _cancellationTokens[context.ExecutionId] = cts;

            try
            {
                GlobalErrorHandler.LogInfo($"Starting workflow execution: {workflow.Name} (ID: {context.ExecutionId})");
                context.ReportProgress($"Starting workflow: {workflow.Name}");

                // Ensure SolidWorks is connected
                if (!_connectionManager.IsConnected)
                {
                    GlobalErrorHandler.LogInfo("Connecting to SolidWorks...");
                    if (!_connectionManager.Connect())
                    {
                        throw new InvalidOperationException("Failed to connect to SolidWorks");
                    }
                }

                // Execute based on execution mode
                switch (workflow.ExecutionMode)
                {
                    case WorkflowExecutionMode.Sequential:
                        await ExecuteSequentialAsync(workflow, context, cts.Token);
                        break;
                    case WorkflowExecutionMode.Parallel:
                        await ExecuteParallelAsync(workflow, context, cts.Token);
                        break;
                    case WorkflowExecutionMode.Conditional:
                        await ExecuteConditionalAsync(workflow, context, cts.Token);
                        break;
                }

                context.Status = WorkflowExecutionStatus.Completed;
                context.EndTime = DateTime.Now;
                GlobalErrorHandler.LogInfo($"Workflow completed successfully: {workflow.Name}");
                context.ReportProgress("Workflow completed successfully", 100);
            }
            catch (OperationCanceledException)
            {
                context.Status = WorkflowExecutionStatus.Cancelled;
                context.EndTime = DateTime.Now;
                GlobalErrorHandler.LogInfo($"Workflow cancelled: {workflow.Name}");
                context.ReportProgress("Workflow cancelled");
            }
            catch (Exception ex)
            {
                context.Status = WorkflowExecutionStatus.Failed;
                context.EndTime = DateTime.Now;
                context.ErrorMessage = ex.Message;
                context.LastException = ex;
                GlobalErrorHandler.LogError(ex, $"Workflow failed: {workflow.Name}");
                context.ReportProgress($"Workflow failed: {ex.Message}");
            }
            finally
            {
                // Cleanup
                _cancellationTokens.Remove(context.ExecutionId);
                cts.Dispose();
            }

            return context;
        }

        private async Task ExecuteSequentialAsync(
            WorkflowDefinition workflow,
            WorkflowExecutionContext context,
            CancellationToken cancellationToken)
        {
            var enabledSteps = workflow.Steps.Where(s => s.IsEnabled).OrderBy(s => s.Order).ToList();

            for (int i = 0; i < enabledSteps.Count; i++)
            {
                cancellationToken.ThrowIfCancellationRequested();
                
                var step = enabledSteps[i];
                context.CurrentStepId = step.Id;
                
                GlobalErrorHandler.LogInfo($"Executing step {i + 1}/{enabledSteps.Count}: {step.Name}");
                context.ReportProgress($"Executing: {step.Name}");

                var result = await ExecuteStepAsync(step, context, cancellationToken);
                context.StepResults.Add(result);
                context.CompletedSteps++;

                if (!result.Success && !step.ContinueOnError)
                {
                    throw new Exception($"Step '{step.Name}' failed: {result.ErrorMessage}");
                }

                // Small delay between steps
                await Task.Delay(100, cancellationToken);
            }
        }

        private async Task ExecuteParallelAsync(
            WorkflowDefinition workflow,
            WorkflowExecutionContext context,
            CancellationToken cancellationToken)
        {
            var enabledSteps = workflow.Steps.Where(s => s.IsEnabled).ToList();
            
            // Group steps by dependencies
            var tasks = enabledSteps.Select(step => ExecuteStepAsync(step, context, cancellationToken));
            
            var results = await Task.WhenAll(tasks);
            
            context.StepResults.AddRange(results);
            context.CompletedSteps = results.Length;

            // Check for failures
            var failures = results.Where(r => !r.Success).ToList();
            if (failures.Any())
            {
                throw new Exception($"{failures.Count} steps failed");
            }
        }

        private async Task ExecuteConditionalAsync(
            WorkflowDefinition workflow,
            WorkflowExecutionContext context,
            CancellationToken cancellationToken)
        {
            // Conditional execution with dependency resolution
            var enabledSteps = workflow.Steps.Where(s => s.IsEnabled).OrderBy(s => s.Order).ToList();
            var executed = new HashSet<string>();

            foreach (var step in enabledSteps)
            {
                cancellationToken.ThrowIfCancellationRequested();

                // Check dependencies
                if (step.DependsOn != null && step.DependsOn.Any())
                {
                    bool dependenciesMet = step.DependsOn.All(dep => executed.Contains(dep));
                    
                    if (!dependenciesMet)
                    {
                        GlobalErrorHandler.LogInfo($"Skipping step '{step.Name}' - dependencies not met");
                        continue;
                    }
                }

                context.CurrentStepId = step.Id;
                var result = await ExecuteStepAsync(step, context, cancellationToken);
                context.StepResults.Add(result);
                context.CompletedSteps++;

                if (result.Success)
                {
                    executed.Add(step.Id);
                }
                else if (!step.ContinueOnError)
                {
                    throw new Exception($"Step '{step.Name}' failed: {result.ErrorMessage}");
                }

                await Task.Delay(100, cancellationToken);
            }
        }

        private async Task<WorkflowStepResult> ExecuteStepAsync(
            WorkflowStep step,
            WorkflowExecutionContext context,
            CancellationToken cancellationToken)
        {
            var result = new WorkflowStepResult
            {
                StepId = step.Id,
                StepName = step.Name
            };

            int attempts = 0;
            int maxAttempts = step.RetryCount + 1;

            while (attempts < maxAttempts)
            {
                result.RetryAttempt = attempts;
                attempts++;

                try
                {
                    cancellationToken.ThrowIfCancellationRequested();

                    // Create timeout cancellation token
                    using (var timeoutCts = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken))
                    {
                        timeoutCts.CancelAfter(TimeSpan.FromSeconds(step.TimeoutSeconds));

                        // Execute step based on type
                        await Task.Run(() => ExecuteStepAction(step, context), timeoutCts.Token);
                    }

                    result.MarkSuccess($"Step completed successfully");
                    return result;
                }
                catch (OperationCanceledException) when (!cancellationToken.IsCancellationRequested)
                {
                    // Timeout occurred
                    if (attempts < maxAttempts)
                    {
                        GlobalErrorHandler.LogInfo($"Step '{step.Name}' timed out. Retry {attempts}/{step.RetryCount}");
                        await Task.Delay(1000, cancellationToken);
                    }
                    else
                    {
                        result.MarkFailure($"Step timed out after {step.TimeoutSeconds} seconds");
                    }
                }
                catch (Exception ex)
                {
                    if (attempts < maxAttempts)
                    {
                        GlobalErrorHandler.LogInfo($"Step '{step.Name}' failed. Retry {attempts}/{step.RetryCount}");
                        await Task.Delay(1000, cancellationToken);
                    }
                    else
                    {
                        result.MarkFailure($"Step failed: {ex.Message}", ex);
                    }
                }
            }

            return result;
        }

        private void ExecuteStepAction(WorkflowStep step, WorkflowExecutionContext context)
        {
            GlobalErrorHandler.LogInfo($"Executing step action: {step.StepType} - {step.ActionName}");

            switch (step.StepType)
            {
                case WorkflowStepType.OpenDocument:
                    ExecuteOpenDocument(step);
                    break;
                case WorkflowStepType.SaveDocument:
                    ExecuteSaveDocument(step);
                    break;
                case WorkflowStepType.CloseDocument:
                    ExecuteCloseDocument(step);
                    break;
                case WorkflowStepType.SetProperty:
                    ExecuteSetProperty(step);
                    break;
                case WorkflowStepType.ModifyFeature:
                    ExecuteModifyFeature(step);
                    break;
                case WorkflowStepType.CreateDrawing:
                    ExecuteCreateDrawing(step);
                    break;
                case WorkflowStepType.CreateAssembly:
                    ExecuteCreateAssembly(step);
                    break;
                case WorkflowStepType.InsertComponent:
                    ExecuteInsertComponent(step);
                    break;
                case WorkflowStepType.ExportDocument:
                    ExecuteExportDocument(step);
                    break;
                default:
                    throw new NotImplementedException($"Step type {step.StepType} not implemented");
            }
        }

        private void ExecuteOpenDocument(WorkflowStep step)
        {
            var filePath = step.Parameters.GetValueOrDefault("FilePath")?.ToString();
            if (string.IsNullOrEmpty(filePath))
            {
                throw new ArgumentException("FilePath parameter is required");
            }

            _documentService.OpenDocument(filePath);
        }

        private void ExecuteSaveDocument(WorkflowStep step)
        {
            _documentService.SaveDocument();
        }

        private void ExecuteCloseDocument(WorkflowStep step)
        {
            _documentService.CloseDocument();
        }

        private void ExecuteSetProperty(WorkflowStep step)
        {
            var propertyName = step.Parameters.GetValueOrDefault("PropertyName")?.ToString();
            var propertyValue = step.Parameters.GetValueOrDefault("PropertyValue")?.ToString();
            var configName = step.Parameters.GetValueOrDefault("ConfigName")?.ToString() ?? "";

            if (string.IsNullOrEmpty(propertyName))
            {
                throw new ArgumentException("PropertyName parameter is required");
            }

            _propertyService.SetProperty(propertyName, propertyValue, configName);
        }

        private void ExecuteModifyFeature(WorkflowStep step)
        {
            var featureName = step.Parameters.GetValueOrDefault("FeatureName")?.ToString();
            var action = step.Parameters.GetValueOrDefault("Action")?.ToString();

            if (string.IsNullOrEmpty(featureName))
            {
                throw new ArgumentException("FeatureName parameter is required");
            }

            switch (action?.ToLower())
            {
                case "suppress":
                    _featureService.SuppressFeature(featureName);
                    break;
                case "unsuppress":
                    _featureService.UnsuppressFeature(featureName);
                    break;
                case "delete":
                    _featureService.DeleteFeature(featureName);
                    break;
                default:
                    throw new ArgumentException($"Unknown action: {action}");
            }
        }

        private void ExecuteCreateDrawing(WorkflowStep step)
        {
            var templatePath = step.Parameters.GetValueOrDefault("TemplatePath")?.ToString();
            if (string.IsNullOrEmpty(templatePath))
            {
                throw new ArgumentException("TemplatePath parameter is required");
            }

            _drawingService.CreateDrawing(templatePath);
        }

        private void ExecuteCreateAssembly(WorkflowStep step)
        {
            var templatePath = step.Parameters.GetValueOrDefault("TemplatePath")?.ToString();
            if (string.IsNullOrEmpty(templatePath))
            {
                throw new ArgumentException("TemplatePath parameter is required");
            }

            _assemblyService.CreateAssembly(templatePath);
        }

        private void ExecuteInsertComponent(WorkflowStep step)
        {
            var componentPath = step.Parameters.GetValueOrDefault("ComponentPath")?.ToString();
            if (string.IsNullOrEmpty(componentPath))
            {
                throw new ArgumentException("ComponentPath parameter is required");
            }

            var x = Convert.ToDouble(step.Parameters.GetValueOrDefault("X", 0.0));
            var y = Convert.ToDouble(step.Parameters.GetValueOrDefault("Y", 0.0));
            var z = Convert.ToDouble(step.Parameters.GetValueOrDefault("Z", 0.0));

            _assemblyService.InsertComponent(componentPath, x, y, z);
        }

        private void ExecuteExportDocument(WorkflowStep step)
        {
            var filePath = step.Parameters.GetValueOrDefault("FilePath")?.ToString();
            var format = step.Parameters.GetValueOrDefault("Format")?.ToString();

            if (string.IsNullOrEmpty(filePath))
            {
                throw new ArgumentException("FilePath parameter is required");
            }

            _documentService.ExportDocument(filePath, format);
        }

        public bool PauseWorkflow(string executionId)
        {
            if (_activeExecutions.TryGetValue(executionId, out var context))
            {
                context.Status = WorkflowExecutionStatus.Paused;
                GlobalErrorHandler.LogInfo($"Workflow paused: {executionId}");
                return true;
            }
            return false;
        }

        public bool ResumeWorkflow(string executionId)
        {
            if (_activeExecutions.TryGetValue(executionId, out var context))
            {
                context.Status = WorkflowExecutionStatus.Running;
                GlobalErrorHandler.LogInfo($"Workflow resumed: {executionId}");
                return true;
            }
            return false;
        }

        public bool CancelWorkflow(string executionId)
        {
            if (_cancellationTokens.TryGetValue(executionId, out var cts))
            {
                cts.Cancel();
                GlobalErrorHandler.LogInfo($"Workflow cancelled: {executionId}");
                return true;
            }
            return false;
        }

        public WorkflowExecutionContext GetExecutionContext(string executionId)
        {
            _activeExecutions.TryGetValue(executionId, out var context);
            return context;
        }
    }
}
