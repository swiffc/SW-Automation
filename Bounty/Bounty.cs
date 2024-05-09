using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using System;
using System.Runtime.InteropServices;

namespace Bounty
{
    internal class Bounty
    {
        private static SldWorks SW = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");
        static void Main()
        {
            
        }
        public static void TurnOffBendLines()
        {
            SW.IActiveDoc2.SetUserPreferenceToggle((int)swUserPreferenceToggle_e.swDisplayBendLines, false);
        }
    }
}
