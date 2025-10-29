
using System;
using System.Collections.Generic;
using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;

namespace UnifiedUI.Services.SolidWorksApi
{
    /// <summary>
    /// Interface for SolidWorks document operations (open, save, close, export)
    /// </summary>
    public interface ISolidWorksDocumentService
    {
        /// <summary>
        /// Opens a SolidWorks document
        /// </summary>
        /// <param name="filePath">Full path to the document</param>
        /// <param name="type">Document type (Part, Assembly, Drawing)</param>
        /// <param name="options">Open options</param>
        /// <returns>Opened document or null if failed</returns>
        ModelDoc2 OpenDocument(string filePath, swDocumentTypes_e type = swDocumentTypes_e.swDocASSEMBLY, 
            swOpenDocOptions_e options = swOpenDocOptions_e.swOpenDocOptions_Silent);

        /// <summary>
        /// Saves the currently active document
        /// </summary>
        /// <param name="options">Save options</param>
        /// <returns>True if successful</returns>
        bool SaveDocument(swSaveAsOptions_e options = swSaveAsOptions_e.swSaveAsOptions_Silent);

        /// <summary>
        /// Saves document with a new name
        /// </summary>
        /// <param name="newFilePath">New file path</param>
        /// <param name="version">SolidWorks version</param>
        /// <param name="options">Save options</param>
        /// <returns>True if successful</returns>
        bool SaveDocumentAs(string newFilePath, swSaveAsVersion_e version = swSaveAsVersion_e.swSaveAsCurrentVersion,
            swSaveAsOptions_e options = swSaveAsOptions_e.swSaveAsOptions_Silent);

        /// <summary>
        /// Closes the specified document
        /// </summary>
        /// <param name="docName">Document name (empty for active document)</param>
        /// <returns>True if successful</returns>
        bool CloseDocument(string docName = "");

        /// <summary>
        /// Closes all documents
        /// </summary>
        /// <returns>True if successful</returns>
        bool CloseAllDocuments();

        /// <summary>
        /// Exports document to specified format
        /// </summary>
        /// <param name="filePath">Export file path</param>
        /// <param name="format">Export format (PDF, DXF, STEP, etc.)</param>
        /// <returns>True if successful</returns>
        bool ExportDocument(string filePath, string format);

        /// <summary>
        /// Gets the currently active document
        /// </summary>
        /// <returns>Active document or null</returns>
        ModelDoc2 GetActiveDocument();

        /// <summary>
        /// Gets all open documents
        /// </summary>
        /// <returns>List of open documents</returns>
        List<ModelDoc2> GetOpenDocuments();

        /// <summary>
        /// Creates a new document from template
        /// </summary>
        /// <param name="templatePath">Path to template file</param>
        /// <param name="type">Document type</param>
        /// <returns>New document or null if failed</returns>
        ModelDoc2 CreateDocumentFromTemplate(string templatePath, swDocumentTypes_e type);

        /// <summary>
        /// Rebuilds the active document
        /// </summary>
        /// <returns>True if successful</returns>
        bool RebuildDocument();

        /// <summary>
        /// Forces rebuild of all features
        /// </summary>
        /// <returns>True if successful</returns>
        bool ForceRebuildAll();
    }
}
