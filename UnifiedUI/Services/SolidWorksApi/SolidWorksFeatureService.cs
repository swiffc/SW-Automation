
using System;
using System.Collections.Generic;
using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using FileTools.Infrastructure;

namespace UnifiedUI.Services.SolidWorksApi
{
    /// <summary>
    /// Implementation of SolidWorks feature manipulation operations
    /// </summary>
    public class SolidWorksFeatureService : ISolidWorksFeatureService
    {
        private readonly ISolidWorksDocumentService _documentService;

        public SolidWorksFeatureService(ISolidWorksDocumentService documentService)
        {
            _documentService = documentService ?? throw new ArgumentNullException(nameof(documentService));
        }

        public List<Feature> GetAllFeatures()
        {
            var features = new List<Feature>();
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null)
            {
                GlobalErrorHandler.LogError("No active document");
                return features;
            }

            try
            {
                Feature feature = (Feature)doc.FirstFeature();
                while (feature != null)
                {
                    features.Add(feature);
                    feature = (Feature)feature.GetNextFeature();
                }

                GlobalErrorHandler.LogInfo($"Found {features.Count} features");
                return features;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Error getting features");
                return features;
            }
        }

        public Feature GetFeatureByName(string featureName)
        {
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null)
            {
                GlobalErrorHandler.LogError("No active document");
                return null;
            }

            try
            {
                var feature = (Feature)doc.FeatureByName(featureName);
                
                if (feature == null)
                {
                    GlobalErrorHandler.LogError($"Feature not found: {featureName}");
                }
                
                return feature;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error getting feature: {featureName}");
                return null;
            }
        }

        public bool SuppressFeature(string featureName)
        {
            var feature = GetFeatureByName(featureName);
            
            if (feature == null)
            {
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Suppressing feature: {featureName}");
                feature.Select(false);
                _documentService.GetActiveDocument().EditSuppress2();
                GlobalErrorHandler.LogInfo($"Feature suppressed: {featureName}");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error suppressing feature: {featureName}");
                return false;
            }
        }

        public bool UnsuppressFeature(string featureName)
        {
            var feature = GetFeatureByName(featureName);
            
            if (feature == null)
            {
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Unsuppressing feature: {featureName}");
                feature.Select(false);
                _documentService.GetActiveDocument().EditUnsuppress2();
                GlobalErrorHandler.LogInfo($"Feature unsuppressed: {featureName}");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error unsuppressing feature: {featureName}");
                return false;
            }
        }

        public bool DeleteFeature(string featureName)
        {
            var feature = GetFeatureByName(featureName);
            
            if (feature == null)
            {
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Deleting feature: {featureName}");
                feature.Select(false);
                _documentService.GetActiveDocument().EditDelete();
                GlobalErrorHandler.LogInfo($"Feature deleted: {featureName}");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error deleting feature: {featureName}");
                return false;
            }
        }

        public bool RenameFeature(string oldName, string newName)
        {
            var feature = GetFeatureByName(oldName);
            
            if (feature == null)
            {
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Renaming feature: {oldName} -> {newName}");
                feature.Name = newName;
                GlobalErrorHandler.LogInfo($"Feature renamed successfully");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error renaming feature: {oldName}");
                return false;
            }
        }

        public bool EditDimension(string dimensionName, double value)
        {
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null)
            {
                GlobalErrorHandler.LogError("No active document");
                return false;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Editing dimension: {dimensionName} = {value}");
                
                var dimension = doc.Parameter(dimensionName);
                
                if (dimension == null)
                {
                    GlobalErrorHandler.LogError($"Dimension not found: {dimensionName}");
                    return false;
                }

                dimension.SystemValue = value;
                GlobalErrorHandler.LogInfo($"Dimension updated successfully");
                return true;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error editing dimension: {dimensionName}");
                return false;
            }
        }

        public Feature CreateSketch(string planeName)
        {
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null)
            {
                GlobalErrorHandler.LogError("No active document");
                return null;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Creating sketch on plane: {planeName}");
                
                // Select the plane
                var plane = doc.FeatureByName(planeName);
                
                if (plane == null)
                {
                    GlobalErrorHandler.LogError($"Plane not found: {planeName}");
                    return null;
                }

                plane.Select(false);
                doc.InsertSketch();
                
                var sketch = doc.GetActiveSketch2();
                
                if (sketch != null)
                {
                    GlobalErrorHandler.LogInfo("Sketch created successfully");
                }
                
                return (Feature)sketch;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error creating sketch on plane: {planeName}");
                return null;
            }
        }

        public Feature CreateExtrude(string sketchName, double depth, bool reverse = false)
        {
            var doc = _documentService.GetActiveDocument();
            
            if (doc == null)
            {
                GlobalErrorHandler.LogError("No active document");
                return null;
            }

            try
            {
                GlobalErrorHandler.LogInfo($"Creating extrude from sketch: {sketchName}, depth: {depth}");
                
                var sketch = GetFeatureByName(sketchName);
                
                if (sketch == null)
                {
                    GlobalErrorHandler.LogError($"Sketch not found: {sketchName}");
                    return null;
                }

                sketch.Select(false);
                
                var featureMgr = (FeatureManager)doc.FeatureManager;
                var feature = featureMgr.FeatureExtrusion2(
                    true, false, reverse,
                    (int)swEndConditions_e.swEndCondBlind, 0,
                    depth, 0,
                    false, false, false, false,
                    0, 0,
                    false, false, false, false,
                    true, true, true
                );

                if (feature != null)
                {
                    GlobalErrorHandler.LogInfo("Extrude created successfully");
                }
                
                return feature;
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, $"Error creating extrude from sketch: {sketchName}");
                return null;
            }
        }
    }
}
