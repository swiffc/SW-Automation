using Bundle.SideFrame.Derived.Children;
using FileTools.Base;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Tools.ModelTools;
using static ModelTools.BendTable;

namespace Bundle.AirSeals
{
    internal abstract class AirSeal : Part
    {
        // Constructor
        protected AirSeal(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Abstract properties
        protected abstract double GapToSeal { get; }


        // Virtual properties
        protected virtual double Width => 3;


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("GapToSeal", "sk:Plate", GapToSeal);
            EditDimension("Width", "sk:Plate", Width);
            EditDimension("Horizontal", "sk:Cope", SideFramePart.Flange + AssemblyClearance - (SideFramePart.THK + AssemblyClearance / 2));
            EditDimension("VerticalCope", "sk:Plate", SideFramePart.THK + GetBendRadius(SideFramePart.THK));
        }


        // Property overrides
        public override bool Enabled => true;
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => "0.1344";
    }
}
