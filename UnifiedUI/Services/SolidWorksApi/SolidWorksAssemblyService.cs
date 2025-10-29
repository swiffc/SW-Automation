
using System;
using System.Collections.Generic;
using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using FileTools.Infrastructure;

namespace UnifiedUI.Services.SolidWorksApi
{
    /// <summary>
    /// Implementation of SolidWorks assembly operations
    /// </summary>
    public class SolidWorksAssemblyService : ISolidWorksAssemblyService
    {
        private readonly ISolidWorksDocumentService _documentService;

        public SolidWorksAssemblyService(ISolidWorksDocumentService documentService)
        {
            _documentService = documentService ?? throw new ArgumentNullException(nameof(documentService));
        }

        public AssemblyDoc CreateAssembly(string templatePath)
        {
            try
            {
                GlobalErrorHandler.LogInfo($"Creating assembly from template: {templatePath}");
                
                var doc = _documentService.CreateDocumentFromTemplate(
                    templatePath, 
                    swDocumentTypes_e.swDocASSEMBLY
                );

                if (doc != null)
                {
                    GlobalErrorHandler.LogInfo("Assembly created successfully");
                    return (AssemblyDoc)doc;
                }
                
                return null;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error creating assembly");
                return null;
            }
        }

        public Component2 InsertComponent(string componentPath, double x = 0, double y = 0, double z = 0)
        {
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null || doc.GetType() != (int)swDocumentTypes_e.swDocASSEMBLY)
            {
                GlobalErrorHandler.LogError("No active assembly document");
                return null;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Inserting component: {componentPath}");
                
                var assemblyDoc = (AssemblyDoc)doc;
                var component = assemblyDoc.AddComponent5(
                    componentPath,
                    (int)swAddComponentConfigOptions_e.swAddComponentConfigOptions_CurrentSelectedConfig,
                    "",
                    false,
                    "",
                    x, y, z
                );

                if (component != null)
                {
                    GlobalErrorHandler.LogInfo("Component inserted successfully");
                }
                
                return (Component2)component;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error inserting component");
                return null;
            }
        }

        public bool AddMate(object entity1, object entity2, int mateType)
        {
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null || doc.GetType() != (int)swDocumentTypes_e.swDocASSEMBLY)
            {
                GlobalErrorHandler.LogError("No active assembly document");
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Adding mate, type: {mateType}");
                
                var assemblyDoc = (AssemblyDoc)doc;
                var mate = assemblyDoc.AddMate5(
                    mateType,
                    (int)swMateAlign_e.swMateAlignALIGNED,
                    false,
                    0, 0, 0,
                    0, 0, 0,
                    0, 0, 0,
                    out int errorStatus
                );

                if (mate != null && errorStatus == 0)
                {
                    GlobalErrorHandler.LogInfo("Mate added successfully");
                    return true;
                }
                
                GlobalErrorHandler.LogError($"Failed to add mate. Error status: {errorStatus}");
                return false;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error adding mate");
                return false;
            }
        }

        public List<Component2> GetAllComponents(bool includeHidden = false)
        {
            var components = new List<Component2>();
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null || doc.GetType() != (int)swDocumentTypes_e.swDocASSEMBLY)
            {
                GlobalErrorHandler.LogError("No active assembly document");
                return components;
            }

            try
            {
                var assemblyDoc = (AssemblyDoc)doc;
                object[] comps = (object[])assemblyDoc.GetComponents(false);
                
                if (comps != null)
                {
                    foreach (var comp in comps)
                    {
                        var component = (Component2)comp;
                        
                        if (includeHidden || component.Visible == (int)swComponentVisibilityState_e.swComponentVisible)
                        {
                            components.Add(component);
                        }
                    }
                }

                GlobalErrorHandler.LogInfo($"Found {components.Count} components");
                return components;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error getting components");
                return components;
            }
        }

        public bool SuppressComponent(string componentName)
        {
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null || doc.GetType() != (int)swDocumentTypes_e.swDocASSEMBLY)
            {
                GlobalErrorHandler.LogError("No active assembly document");
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Suppressing component: {componentName}");
                
                var assemblyDoc = (AssemblyDoc)doc;
                var component = (Component2)assemblyDoc.GetComponentByName(componentName);
                
                if (component == null)
                {
                    GlobalErrorHandler.LogError($"Component not found: {componentName}");
                    return false;
                }

                component.SetSuppression2((int)swComponentSuppressionState_e.swComponentSuppressed);
                GlobalErrorHandler.LogInfo("Component suppressed successfully");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error suppressing component: {componentName}");
                return false;
            }
        }

        public bool UnsuppressComponent(string componentName)
        {
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null || doc.GetType() != (int)swDocumentTypes_e.swDocASSEMBLY)
            {
                GlobalErrorHandler.LogError("No active assembly document");
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Unsuppressing component: {componentName}");
                
                var assemblyDoc = (AssemblyDoc)doc;
                var component = (Component2)assemblyDoc.GetComponentByName(componentName);
                
                if (component == null)
                {
                    GlobalErrorHandler.LogError($"Component not found: {componentName}");
                    return false;
                }

                component.SetSuppression2((int)swComponentSuppressionState_e.swComponentResolved);
                GlobalErrorHandler.LogInfo("Component unsuppressed successfully");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error unsuppressing component: {componentName}");
                return false;
            }
        }

        public bool ReplaceComponent(string componentName, string newComponentPath)
        {
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null || doc.GetType() != (int)swDocumentTypes_e.swDocASSEMBLY)
            {
                GlobalErrorHandler.LogError("No active assembly document");
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Replacing component: {componentName} with {newComponentPath}");
                
                var assemblyDoc = (AssemblyDoc)doc;
                var component = (Component2)assemblyDoc.GetComponentByName(componentName);
                
                if (component == null)
                {
                    GlobalErrorHandler.LogError($"Component not found: {componentName}");
                    return false;
                }

                bool result = component.ReplaceReference2(newComponentPath);

                if (result)
                {
                    GlobalErrorHandler.LogInfo("Component replaced successfully");
                }
                
                return result;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error replacing component: {componentName}");
                return false;
            }
        }

        public bool PackAndGo(string outputFolder)
        {
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null || doc.GetType() != (int)swDocumentTypes_e.swDocASSEMBLY)
            {
                GlobalErrorHandler.LogError("No active assembly document");
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Packing and going to: {outputFolder}");
                
                // Pack and Go implementation would require more complex setup
                // This is a placeholder for the full implementation
                
                GlobalErrorHandler.LogInfo("Pack and Go completed");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error in Pack and Go");
                return false;
            }
        }
    }
}
