using SolidWorks.Interop.sldworks;
using System;

namespace Automation_Library
{
    public class Dimensions : Views
    {
        public void AlignDimensions()
        {
            foreach (IView view in viewsArray)
            {
                drawingDoc.ActivateView(view.Name);
                try
                {
                    DisplayDimension[] dimensionsArray = Array.ConvertAll(
                        (object[])view.GetDisplayDimensions(), p => (DisplayDimension)p);
                    modelDoc2.Extension.MultiSelect(dimensionsArray, true, null);
                }
                catch (Exception) { }
            }
            modelDoc2.Extension.AlignDimensions(0, 1);
            modelDoc2.ClearSelection();
        }
        public void DeleteDanglingAnnotations()
        {
            modelDoc2.ClearSelection();
            foreach (IView view in viewsArray)
            {
                drawingDoc.ActivateView(view.Name);
                try
                {
                    IAnnotation[] annotationsArray = Array.ConvertAll(
                        (object[])view.GetAnnotations(), p => (Annotation)p);
                    foreach (IAnnotation annotation in annotationsArray)
                    {
                        if (annotation.IsDangling())
                        {
                            annotation.Select(true);
                        }
                    }
                }
                catch (Exception) { }
            }
            modelDoc2.Extension.DeleteSelection2(0);
        }
    }
}
