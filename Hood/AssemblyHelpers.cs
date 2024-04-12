using EPDM.Interop.epdm;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using cTools = ModelTools.ReleaseCOM;
using mTools = Tools.ModelTools;

namespace Hood
{
    internal class AssemblyHelpers
    {
        public class StandardParts
        {
            public string RingString { get; private set; }
            public string GuardString { get; private set; }

            public StandardParts(string ringString, string guardNumber)
            {
                RingString = ringString;
                GuardString = guardNumber;
            }
        }

        public static Dictionary<double, (double Quantity, double ArcLength)>
        FanHoles = new Dictionary<double, (double Quantity, double ArcLength)>
        { //dia  qty  ArcLength
            {5,  (4,  14.4375)},
            {6,  (5,  13.1875)},
            {7,  (5,  15.5625)},
            {8,  (6,  14.3125)},
            {9,  (6,  16.1875)},
            {10, (7,  15.0625)},
            {11, (7,  16.6250)},
            {12, (8,  15.6250)},
            {13, (8,  16.9375)},
            {14, (9,  16.0000)},
            {15, (10, 15.3125)},
            {16, (10, 16.3125)}
        };
    }
}
