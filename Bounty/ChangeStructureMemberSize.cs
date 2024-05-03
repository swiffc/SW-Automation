using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using System;
using System.Runtime.InteropServices;

namespace Bounty
{
    internal class ChangeStructureMemberSize
    {
        static void Main()
        {
            SldWorks SW = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");
            ModelDoc2 modelDoc2 = SW.IActiveDoc2;
            var selectionMgr = modelDoc2.SelectionManager;

            string newSize = "W8x28";

            Feature feature = selectionMgr.GetSelectedObject6(1, -1) as Feature;
            IStructuralMemberFeatureData member = feature.GetDefinition() as IStructuralMemberFeatureData;

            Console.WriteLine($"Attempting to set {member.ConfigurationName} to {newSize}");
            member.ConfigurationName = newSize;
            Console.WriteLine($"  Result: {member.ConfigurationName}");
        }

        public static void TurnOffBendLines(ModelDoc2 modelDoc2)
        {
            modelDoc2.SetUserPreferenceToggle((int)swUserPreferenceToggle_e.swDisplayBendLines, false);
        }
    }
}
