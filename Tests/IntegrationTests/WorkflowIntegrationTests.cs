
using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Xunit;
using FluentAssertions;
using UnifiedUI.Models.Workflow;
using UnifiedUI.Services.Workflow;
using SolidWorksAutomation.Tests.Mocks;

namespace SolidWorksAutomation.Tests.IntegrationTests
{
    /// <summary>
    /// Integration tests for workflow execution
    /// </summary>
    public class WorkflowIntegrationTests
    {
        [Fact]
        public async Task WorkflowPersistenceService_ShouldSaveAndLoadWorkflow()
        {
            // Arrange
            var persistenceService = new WorkflowPersistenceService();
            var workflow = new WorkflowDefinition
            {
                Name = "Integration Test Workflow",
                Description = "Test workflow for integration testing",
                Steps = new List<WorkflowStep>
                {
                    new WorkflowStep
                    {
                        Name = "Test Step",
                        ActionName = "TestAction",
                        StepType = WorkflowStepType.SaveDocument
                    }
                }
            };

            try
            {
                // Act
                var saveResult = persistenceService.SaveWorkflow(workflow);
                var loadedWorkflow = persistenceService.LoadWorkflow(workflow.Id);

                // Assert
                saveResult.Should().BeTrue();
                loadedWorkflow.Should().NotBeNull();
                loadedWorkflow.Name.Should().Be(workflow.Name);
                loadedWorkflow.Steps.Should().HaveCount(1);
            }
            finally
            {
                // Cleanup
                persistenceService.DeleteWorkflow(workflow.Id);
            }
        }

        [Fact]
        public void WorkflowTemplateService_ShouldProvideBuiltInTemplates()
        {
            // Arrange
            var persistenceService = new WorkflowPersistenceService();
            var templateService = new WorkflowTemplateService(persistenceService);

            // Act
            var templates = templateService.GetAllTemplates();

            // Assert
            templates.Should().NotBeEmpty();
            templates.Should().Contain(t => t.TemplateName == "SimpleDocumentProcessing");
            templates.Should().Contain(t => t.TemplateName == "BatchExportPDF");
        }

        [Fact]
        public void WorkflowTemplateService_ShouldCreateWorkflowFromTemplate()
        {
            // Arrange
            var persistenceService = new WorkflowPersistenceService();
            var templateService = new WorkflowTemplateService(persistenceService);

            // Act
            var workflow = templateService.CreateFromTemplate("SimpleDocumentProcessing", "My Test Workflow");

            // Assert
            workflow.Should().NotBeNull();
            workflow.Name.Should().Be("My Test Workflow");
            workflow.TemplateName.Should().Be("SimpleDocumentProcessing");
            workflow.IsTemplate.Should().BeFalse();
            workflow.Steps.Should().NotBeEmpty();
        }
    }
}
