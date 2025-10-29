
using System;
using System.Collections.Generic;
using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using FileTools.Infrastructure;

namespace UnifiedUI.Services.SolidWorksApi
{
    /// <summary>
    /// Implementation of SolidWorks drawing automation operations
    /// </summary>
    public class SolidWorksDrawingService : ISolidWorksDrawingService
    {
        private readonly ISolidWorksDocumentService _documentService;

        public SolidWorksDrawingService(ISolidWorksDocumentService documentService)
        {
            _documentService = documentService ?? throw new ArgumentNullException(nameof(documentService));
        }

        public DrawingDoc CreateDrawing(string templatePath)
        {
            try
            {
                GlobalErrorHandler.LogInfo($"Creating drawing from template: {templatePath}");
                
                var doc = _documentService.CreateDocumentFromTemplate(
                    templatePath, 
                    swDocumentTypes_e.swDocDRAWING
                );

                if (doc != null)
                {
                    GlobalErrorHandler.LogInfo("Drawing created successfully");
                    return (DrawingDoc)doc;
                }
                
                return null;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error creating drawing");
                return null;
            }
        }

        public bool InsertModelView(string modelPath, double x, double y, double scale = 1.0)
        {
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null || doc.GetType() != (int)swDocumentTypes_e.swDocDRAWING)
            {
                GlobalErrorHandler.LogError("No active drawing document");
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Inserting model view: {modelPath}");
                
                var drawingDoc = (DrawingDoc)doc;
                var view = drawingDoc.CreateDrawViewFromModelView3(
                    modelPath, 
                    "*Front", 
                    x, y, 0
                );

                if (view != null)
                {
                    view.ScaleRatio = new double[] { scale, 1 };
                    GlobalErrorHandler.LogInfo("Model view inserted successfully");
                    return true;
                }
                
                return false;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error inserting model view");
                return false;
            }
        }

        public bool CreateStandard3Views(string modelPath)
        {
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null || doc.GetType() != (int)swDocumentTypes_e.swDocDRAWING)
            {
                GlobalErrorHandler.LogError("No active drawing document");
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Creating standard 3 views for: {modelPath}");
                
                var drawingDoc = (DrawingDoc)doc;
                
                // Get the first sheet
                var sheet = (Sheet)drawingDoc.GetCurrentSheet();
                
                if (sheet == null)
                {
                    GlobalErrorHandler.LogError("No active sheet");
                    return false;
                }

                // Create standard 3 views
                var view = drawingDoc.Create3rdAngleViews2(modelPath);

                if (view != null)
                {
                    GlobalErrorHandler.LogInfo("Standard 3 views created successfully");
                    return true;
                }
                
                return false;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error creating standard 3 views");
                return false;
            }
        }

        public bool AddDimensions()
        {
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null || doc.GetType() != (int)swDocumentTypes_e.swDocDRAWING)
            {
                GlobalErrorHandler.LogError("No active drawing document");
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo("Adding dimensions");
                
                var drawingDoc = (DrawingDoc)doc;
                drawingDoc.InsertModelAnnotations3(
                    (int)swImportModelItemsSource_e.swImportModelItemsFromEntireModel,
                    (int)swInsertAnnotation_e.swInsertDimension,
                    true, false, false, false
                );

                GlobalErrorHandler.LogInfo("Dimensions added successfully");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error adding dimensions");
                return false;
            }
        }

        public List<string> GetAllSheets()
        {
            var sheets = new List<string>();
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null || doc.GetType() != (int)swDocumentTypes_e.swDocDRAWING)
            {
                GlobalErrorHandler.LogError("No active drawing document");
                return sheets;
            }

            try
            {
                var drawingDoc = (DrawingDoc)doc;
                object[] sheetNames = (object[])drawingDoc.GetSheetNames();
                
                if (sheetNames != null)
                {
                    foreach (string name in sheetNames)
                    {
                        sheets.Add(name);
                    }
                }

                GlobalErrorHandler.LogInfo($"Found {sheets.Count} sheets");
                return sheets;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error getting sheets");
                return sheets;
            }
        }

        public bool ActivateSheet(string sheetName)
        {
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null || doc.GetType() != (int)swDocumentTypes_e.swDocDRAWING)
            {
                GlobalErrorHandler.LogError("No active drawing document");
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Activating sheet: {sheetName}");
                
                var drawingDoc = (DrawingDoc)doc;
                bool result = drawingDoc.ActivateSheet(sheetName);

                if (result)
                {
                    GlobalErrorHandler.LogInfo("Sheet activated successfully");
                }
                
                return result;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error activating sheet: {sheetName}");
                return false;
            }
        }

        public bool AddSheet(string sheetName)
        {
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null || doc.GetType() != (int)swDocumentTypes_e.swDocDRAWING)
            {
                GlobalErrorHandler.LogError("No active drawing document");
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Adding sheet: {sheetName}");
                
                var drawingDoc = (DrawingDoc)doc;
                var sheet = drawingDoc.NewSheet4(
                    sheetName,
                    (int)swDwgPaperSizes_e.swDwgPaperAsize,
                    (int)swDwgTemplates_e.swDwgTemplateAsize,
                    1.0, 1.0,
                    "", "", "", ""
                );

                if (sheet != null)
                {
                    GlobalErrorHandler.LogInfo("Sheet added successfully");
                    return true;
                }
                
                return false;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error adding sheet: {sheetName}");
                return false;
            }
        }
    }
}
