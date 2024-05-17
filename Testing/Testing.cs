using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using System.Runtime.InteropServices;


namespace Testing
{
    internal class Testing
    {
        private static SldWorks SW = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");
        static void Main()
        {
            Component2 component = SW.IActiveDoc2.SelectionManager.GetSelectedObject6(1, -1) as Component2;
            SW.IActiveDoc2.ClearSelection2(true);

            object[] matesObj = component.GetMates();
            Mate2[] mates = new Mate2[matesObj.Length];

            for (int i = 0; i < mates.Length; i++)
            {
                mates[i] = matesObj[i] as Mate2;
            }

            SW.IActiveDoc2.Extension.MultiSelect2(mates, false, null);
            SW.IActiveDoc2.Extension.DeleteSelection2((int)swDeleteSelectionOptions_e.swDelete_Absorbed);
        }

    }
}
