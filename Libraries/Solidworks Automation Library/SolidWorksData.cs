using SolidWorks.Interop.sldworks;

namespace Automation_Library
{
    public class SolidWorksData
    {
        public static ISldWorks sldWorks = new SldWorks();
        public static IModelDoc2 modelDoc2 = (IModelDoc2)sldWorks.ActiveDoc;
    }
}
