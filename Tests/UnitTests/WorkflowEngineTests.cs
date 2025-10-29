
using System;
using System.Threading.Tasks;
using Xunit;
using Moq;
using FluentAssertions;
using UnifiedUI.Models.Workflow;
using UnifiedUI.Services.Workflow;
using UnifiedUI.Services.SolidWorksApi;

namespace SolidWorksAutomation.Tests.UnitTests
{
    /// <summary>
    /// Unit tests for WorkflowEngine
    /// </summary>
    public class WorkflowEngineTests
    {
        private readonly Mock<ISolidWorksConnectionManager> _mockConnectionManager;
        private readonly Mock<ISolidWorksDocumentService> _mockDocumentService;
        private readonly Mock<ISolidWorksFeatureService> _mockFeatureService;
        private readonly Mock<ISolidWorksPropertyService> _mockPropertyService;
        private readonly Mock<ISolidWorksDrawingService> _mockDrawingService;
        private readonly Mock<ISolidWorksAssemblyService> _mockAssemblyService;
        private readonly WorkflowEngine _workflowEngine;

        public WorkflowEngineTests()
        {
            _mockConnectionManager = new Mock<ISolidWorksConnectionManager>();
            _mockDocumentService = new Mock<ISolidWorksDocumentService>();
            _mockFeatureService = new Mock<ISolidWorksFeatureService>();
            _mockPropertyService = new Mock<ISolidWorksPropertyService>();
            _mockDrawingService = new Mock<ISolidWorksDrawingService>();
            _mockAssemblyService = new Mock<ISolidWorksAssemblyService>();

            _workflowEngine = new WorkflowEngine(
                _mockConnectionManager.Object,
                _mockDocumentService.Object,
                _mockFeatureService.Object,
                _mockPropertyService.Object,
                _mockDrawingService.Object,
                _mockAssemblyService.Object
            );
        }

        [Fact]
        public void Constructor_ShouldThrowArgumentNullException_WhenConnectionManagerIsNull()
        {
            // Act & Assert
            Assert.Throws<ArgumentNullException>(() => new WorkflowEngine(
                null,
                _mockDocumentService.Object,
                _mockFeatureService.Object,
                _mockPropertyService.Object,
                _mockDrawingService.Object,
                _mockAssemblyService.Object
            ));
        }

        [Fact]
        public async Task ExecuteWorkflowAsync_ShouldConnectToSolidWorks_WhenNotConnected()
        {
            // Arrange
            _mockConnectionManager.Setup(m => m.IsConnected).Returns(false);
            _mockConnectionManager.Setup(m => m.Connect(It.IsAny<bool>())).Returns(true);

            var workflow = new WorkflowDefinition
            {
                Name = "Test Workflow",
                Steps = new System.Collections.Generic.List<WorkflowStep>
                {
                    new WorkflowStep
                    {
                        Name = "Test Step",
                        ActionName = "TestAction",
                        StepType = WorkflowStepType.SaveDocument,
                        IsEnabled = true
                    }
                }
            };

            // Act
            await _workflowEngine.ExecuteWorkflowAsync(workflow);

            // Assert
            _mockConnectionManager.Verify(m => m.Connect(It.IsAny<bool>()), Times.Once);
        }

        [Fact]
        public async Task ExecuteWorkflowAsync_ShouldThrowException_WhenWorkflowIsInvalid()
        {
            // Arrange
            var workflow = new WorkflowDefinition
            {
                Name = "",  // Invalid - empty name
                Steps = new System.Collections.Generic.List<WorkflowStep>()
            };

            // Act & Assert
            await Assert.ThrowsAsync<InvalidOperationException>(
                () => _workflowEngine.ExecuteWorkflowAsync(workflow)
            );
        }

        [Fact]
        public async Task ExecuteWorkflowAsync_ShouldExecuteAllSteps_WhenWorkflowIsValid()
        {
            // Arrange
            _mockConnectionManager.Setup(m => m.IsConnected).Returns(true);
            _mockDocumentService.Setup(m => m.SaveDocument(It.IsAny<SolidWorks.Interop.swconst.swSaveAsOptions_e>()))
                .Returns(true);

            var workflow = new WorkflowDefinition
            {
                Name = "Test Workflow",
                ExecutionMode = WorkflowExecutionMode.Sequential,
                Steps = new System.Collections.Generic.List<WorkflowStep>
                {
                    new WorkflowStep
                    {
                        Name = "Save Document",
                        ActionName = "SaveDocument",
                        StepType = WorkflowStepType.SaveDocument,
                        IsEnabled = true,
                        Order = 1
                    }
                }
            };

            // Act
            var result = await _workflowEngine.ExecuteWorkflowAsync(workflow);

            // Assert
            result.Should().NotBeNull();
            result.Status.Should().Be(WorkflowExecutionStatus.Completed);
            result.CompletedSteps.Should().Be(1);
        }

        [Fact]
        public void PauseWorkflow_ShouldReturnFalse_WhenExecutionIdNotFound()
        {
            // Act
            var result = _workflowEngine.PauseWorkflow("non-existent-id");

            // Assert
            result.Should().BeFalse();
        }

        [Fact]
        public void GetExecutionContext_ShouldReturnNull_WhenExecutionIdNotFound()
        {
            // Act
            var result = _workflowEngine.GetExecutionContext("non-existent-id");

            // Assert
            result.Should().BeNull();
        }
    }
}
