using FileTools.Base;
using ModelTools;
using Structure.Braces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Structure.Columns.Derived.Children
{
    public abstract class Clip : Part
    {
        // Static properties
        static public double EndPanelShift => Clip_THK / 2 + EndPanel_THK;
        static public double SidePanelShift => Clip_THK / 2 + SidePanel_THK;


        // Constructor
        protected Clip(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Diameter", "sk:Plate", HoleDiameter_Structural);
            EditDimension("HoleToEdge", "sk:Plate", HoleToEdge);
            EditDimension("THK", "Plate", Clip_THK);
        }

    }
}
