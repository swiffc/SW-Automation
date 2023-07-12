using SolidWorks.Interop.sldworks;
using System;

namespace Automation_Library
{
    public class Views : CurrentSheet
    {
        public static IView[] viewsArray = Array.ConvertAll(
            (object[])sheet.GetViews(), p => (IView)p);
    }
}
