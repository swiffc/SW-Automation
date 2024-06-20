using Bundle.SideFrame.Derived.Children;
using Bundle.TubeSupports.Children;
using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using static Tools.ModelTools;

namespace Bundle.TubeKeepers.Children
{
    internal class TubeKeeperPipe : Part
    {
        // Static properties
        static public double OD => TubeKeeperWeldment.IsLarge ? 2.875 : 1.9;
        static public double Length => Bundle.Width - SideFramePart.THK * 2 - AssemblyClearance - TubeSupport_EndPlate.THK * 2;


        // Constructor
        public TubeKeeperPipe(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Diameter", "sk:Pipe", OD);
            EditDimension("Length", "Pipe", Length);
            EditDimension("Wall", "Pipe", TubeKeeperWeldment.IsLarge ? 0.203 : 0.145);
        }


        // Property overrides
        public override bool Enabled => !IsSmithco;
        public override string StaticPartNo => "1504P";
        public override Shape RawMaterialShape => Shape.Pipe;
        public override string SizeOrThickness => "1.5_Sch40";
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(),
                };
            }
        }

    }
}
