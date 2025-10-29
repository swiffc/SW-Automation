
using System;
using System.Collections.Generic;
using SolidWorks.Interop.sldworks;
using FileTools.Infrastructure;

namespace UnifiedUI.Services.SolidWorksApi
{
    /// <summary>
    /// Implementation of SolidWorks custom property operations
    /// </summary>
    public class SolidWorksPropertyService : ISolidWorksPropertyService
    {
        private readonly ISolidWorksDocumentService _documentService;

        public SolidWorksPropertyService(ISolidWorksDocumentService documentService)
        {
            _documentService = documentService ?? throw new ArgumentNullException(nameof(documentService));
        }

        public string GetProperty(string propertyName, string configName = "")
        {
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null)
            {
                GlobalErrorHandler.LogError("No active document");
                return "";
            }

            try
            {
                var extension = doc.Extension;
                var customPropertyManager = extension.get_CustomPropertyManager(configName);

                string value = "";
                string resolvedValue = "";
                bool wasResolved = false;
                
                customPropertyManager.Get5(propertyName, false, out value, out resolvedValue, out wasResolved);
                
                return resolvedValue ?? value ?? "";
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error getting property: {propertyName}");
                return "";
            }
        }

        public bool SetProperty(string propertyName, string value, string configName = "")
        {
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null)
            {
                GlobalErrorHandler.LogError("No active document");
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Setting property: {propertyName} = {value}");
                
                var extension = doc.Extension;
                var customPropertyManager = extension.get_CustomPropertyManager(configName);

                int result = customPropertyManager.Add3(
                    propertyName,
                    (int)swCustomInfoType_e.swCustomInfoText,
                    value,
                    (int)swCustomPropertyAddOption_e.swCustomPropertyReplaceValue
                );

                if (result != 0)
                {
                    GlobalErrorHandler.LogInfo("Property set successfully");
                    return true;
                }
                else
                {
                    GlobalErrorHandler.LogError("Failed to set property");
                    return false;
                }
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error setting property: {propertyName}");
                return false;
            }
        }

        public bool DeleteProperty(string propertyName, string configName = "")
        {
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null)
            {
                GlobalErrorHandler.LogError("No active document");
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Deleting property: {propertyName}");
                
                var extension = doc.Extension;
                var customPropertyManager = extension.get_CustomPropertyManager(configName);

                int result = customPropertyManager.Delete(propertyName);

                if (result != 0)
                {
                    GlobalErrorHandler.LogInfo("Property deleted successfully");
                    return true;
                }
                else
                {
                    GlobalErrorHandler.LogError("Failed to delete property");
                    return false;
                }
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error deleting property: {propertyName}");
                return false;
            }
        }

        public Dictionary<string, string> GetAllProperties(string configName = "")
        {
            var properties = new Dictionary<string, string>();
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null)
            {
                GlobalErrorHandler.LogError("No active document");
                return properties;
            }

            try
            {
                var extension = doc.Extension;
                var customPropertyManager = extension.get_CustomPropertyManager(configName);

                object[] names = (object[])customPropertyManager.GetNames();
                
                if (names != null)
                {
                    foreach (string name in names)
                    {
                        string value = GetProperty(name, configName);
                        properties[name] = value;
                    }
                }

                GlobalErrorHandler.LogInfo($"Retrieved {properties.Count} properties");
                return properties;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error getting all properties");
                return properties;
            }
        }

        public bool SetMultipleProperties(Dictionary<string, string> properties, string configName = "")
        {
            if (properties == null || properties.Count == 0)
            {
                return true;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Setting {properties.Count} properties");
                
                bool allSuccessful = true;
                
                foreach (var kvp in properties)
                {
                    if (!SetProperty(kvp.Key, kvp.Value, configName))
                    {
                        allSuccessful = false;
                    }
                }

                return allSuccessful;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error setting multiple properties");
                return false;
            }
        }

        public bool ClearAllProperties(string configName = "")
        {
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null)
            {
                GlobalErrorHandler.LogError("No active document");
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo("Clearing all properties");
                
                var extension = doc.Extension;
                var customPropertyManager = extension.get_CustomPropertyManager(configName);

                object[] names = (object[])customPropertyManager.GetNames();
                
                if (names != null)
                {
                    foreach (string name in names)
                    {
                        customPropertyManager.Delete(name);
                    }
                }

                GlobalErrorHandler.LogInfo("All properties cleared");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error clearing all properties");
                return false;
            }
        }
    }
}
