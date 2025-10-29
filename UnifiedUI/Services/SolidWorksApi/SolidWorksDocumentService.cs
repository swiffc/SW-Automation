
using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.InteropServices;
using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using FileTools.Infrastructure;

namespace UnifiedUI.Services.SolidWorksApi
{
    /// <summary>
    /// Implementation of SolidWorks document operations
    /// </summary>
    public class SolidWorksDocumentService : ISolidWorksDocumentService
    {
        private readonly ISolidWorksConnectionManager _connectionManager;

        public SolidWorksDocumentService(ISolidWorksConnectionManager connectionManager)
        {
            _connectionManager = connectionManager ?? throw new ArgumentNullException(nameof(connectionManager));
        }

        public ModelDoc2 OpenDocument(string filePath, swDocumentTypes_e type = swDocumentTypes_e.swDocASSEMBLY, 
            swOpenDocOptions_e options = swOpenDocOptions_e.swOpenDocOptions_Silent)
        {
            if (!_connectionManager.IsConnected)
            {
                GlobalErrorHandler.LogError("Not connected to SolidWorks");
                return null;
            }

            if (string.IsNullOrWhiteSpace(filePath) || !File.Exists(filePath))
            {
                GlobalErrorHandler.LogError($"File not found: {filePath}");
                return null;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Opening document: {filePath}");
                
                int errors = 0;
                int warnings = 0;

                var doc = _connectionManager.SwApp.OpenDoc6(
                    filePath,
                    (int)type,
                    (int)options,
                    "",
                    ref errors,
                    ref warnings
                );

                if (doc == null || errors != 0)
                {
                    GlobalErrorHandler.LogError($"Failed to open document. Errors: {errors}, Warnings: {warnings}");
                    return null;
                }

                GlobalErrorHandler.LogInfo($"Document opened successfully: {Path.GetFileName(filePath)}");
                return doc;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error opening document: {filePath}");
                return null;
            }
        }

        public bool SaveDocument(swSaveAsOptions_e options = swSaveAsOptions_e.swSaveAsOptions_Silent)
        {
            var doc = GetActiveDocument();
            if (doc == null)
            {
                GlobalErrorHandler.LogError("No active document to save");
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Saving document: {doc.GetTitle()}");
                
                int errors = 0;
                int warnings = 0;
                bool result = doc.Save3((int)options, ref errors, ref warnings);

                if (!result || errors != 0)
                {
                    GlobalErrorHandler.LogError($"Failed to save document. Errors: {errors}, Warnings: {warnings}");
                    return false;
                }

                GlobalErrorHandler.LogInfo("Document saved successfully");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error saving document");
                return false;
            }
        }

        public bool SaveDocumentAs(string newFilePath, swSaveAsVersion_e version = swSaveAsVersion_e.swSaveAsCurrentVersion,
            swSaveAsOptions_e options = swSaveAsOptions_e.swSaveAsOptions_Silent)
        {
            var doc = GetActiveDocument();
            if (doc == null)
            {
                GlobalErrorHandler.LogError("No active document to save");
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Saving document as: {newFilePath}");
                
                // Ensure directory exists
                string directory = Path.GetDirectoryName(newFilePath);
                if (!Directory.Exists(directory))
                {
                    Directory.CreateDirectory(directory);
                }

                int errors = 0;
                int warnings = 0;
                bool result = doc.SaveAs4(newFilePath, (int)version, (int)options, ref errors, ref warnings);

                if (!result || errors != 0)
                {
                    GlobalErrorHandler.LogError($"Failed to save document as. Errors: {errors}, Warnings: {warnings}");
                    return false;
                }

                GlobalErrorHandler.LogInfo($"Document saved as: {Path.GetFileName(newFilePath)}");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error saving document as: {newFilePath}");
                return false;
            }
        }

        public bool CloseDocument(string docName = "")
        {
            try
            {
                if (string.IsNullOrWhiteSpace(docName))
                {
                    var doc = GetActiveDocument();
                    if (doc != null)
                    {
                        docName = doc.GetTitle();
                    }
                }

                if (string.IsNullOrWhiteSpace(docName))
                {
                    GlobalErrorHandler.LogError("No document to close");
                    return false;
                }

                GlobalErrorHandler.LogInfo($"Closing document: {docName}");
                _connectionManager.SwApp.CloseDoc(docName);
                GlobalErrorHandler.LogInfo("Document closed successfully");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error closing document");
                return false;
            }
        }

        public bool CloseAllDocuments()
        {
            try
            {
                GlobalErrorHandler.LogInfo("Closing all documents");
                _connectionManager.SwApp.CloseAllDocuments(true);
                GlobalErrorHandler.LogInfo("All documents closed");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error closing all documents");
                return false;
            }
        }

        public bool ExportDocument(string filePath, string format)
        {
            var doc = GetActiveDocument();
            if (doc == null)
            {
                GlobalErrorHandler.LogError("No active document to export");
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Exporting document to: {filePath} ({format})");
                
                // Ensure directory exists
                string directory = Path.GetDirectoryName(filePath);
                if (!Directory.Exists(directory))
                {
                    Directory.CreateDirectory(directory);
                }

                int errors = 0;
                int warnings = 0;
                bool result = doc.Extension.SaveAs(filePath, (int)swSaveAsVersion_e.swSaveAsCurrentVersion, 
                    (int)swSaveAsOptions_e.swSaveAsOptions_Silent, null, ref errors, ref warnings);

                if (!result || errors != 0)
                {
                    GlobalErrorHandler.LogError($"Failed to export document. Errors: {errors}, Warnings: {warnings}");
                    return false;
                }

                GlobalErrorHandler.LogInfo($"Document exported successfully: {Path.GetFileName(filePath)}");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error exporting document: {filePath}");
                return false;
            }
        }

        public ModelDoc2 GetActiveDocument()
        {
            if (!_connectionManager.IsConnected)
            {
                return null;
            }

            try
            {
                return (ModelDoc2)_connectionManager.SwApp.ActiveDoc;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error getting active document");
                return null;
            }
        }

        public List<ModelDoc2> GetOpenDocuments()
        {
            var documents = new List<ModelDoc2>();

            if (!_connectionManager.IsConnected)
            {
                return documents;
            }

            try
            {
                object[] modelDocs = (object[])_connectionManager.SwApp.GetDocuments();
                
                if (modelDocs != null)
                {
                    foreach (var doc in modelDocs)
                    {
                        if (doc is ModelDoc2 modelDoc)
                        {
                            documents.Add(modelDoc);
                        }
                    }
                }

                GlobalErrorHandler.LogInfo($"Found {documents.Count} open documents");
                return documents;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error getting open documents");
                return documents;
            }
        }

        public ModelDoc2 CreateDocumentFromTemplate(string templatePath, swDocumentTypes_e type)
        {
            if (!_connectionManager.IsConnected)
            {
                GlobalErrorHandler.LogError("Not connected to SolidWorks");
                return null;
            }

            if (string.IsNullOrWhiteSpace(templatePath) || !File.Exists(templatePath))
            {
                GlobalErrorHandler.LogError($"Template not found: {templatePath}");
                return null;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Creating document from template: {templatePath}");
                
                var doc = _connectionManager.SwApp.NewDocument(templatePath, (int)type, 0, 0);

                if (doc == null)
                {
                    GlobalErrorHandler.LogError("Failed to create document from template");
                    return null;
                }

                GlobalErrorHandler.LogInfo("Document created successfully from template");
                return (ModelDoc2)doc;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error creating document from template: {templatePath}");
                return null;
            }
        }

        public bool RebuildDocument()
        {
            var doc = GetActiveDocument();
            if (doc == null)
            {
                GlobalErrorHandler.LogError("No active document to rebuild");
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo("Rebuilding document");
                doc.ForceRebuild3(false);
                GlobalErrorHandler.LogInfo("Document rebuilt successfully");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error rebuilding document");
                return false;
            }
        }

        public bool ForceRebuildAll()
        {
            var doc = GetActiveDocument();
            if (doc == null)
            {
                GlobalErrorHandler.LogError("No active document to rebuild");
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo("Force rebuilding all features");
                doc.ForceRebuild3(true);
                GlobalErrorHandler.LogInfo("All features rebuilt successfully");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error force rebuilding all");
                return false;
            }
        }
    }
}
