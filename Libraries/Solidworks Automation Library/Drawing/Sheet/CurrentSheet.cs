using SolidWorks.Interop.sldworks;

namespace Automation_Library
{
    public class CurrentSheet : Drawing
    {
        public static ISheet sheet = (ISheet)drawingDoc.GetCurrentSheet();
    }
}
