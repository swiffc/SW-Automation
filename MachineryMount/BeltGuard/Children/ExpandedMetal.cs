using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using static ModelTools.BendTable;
using static Tools.ModelTools;

namespace MachineryMount.BeltGuard.Children
{
    internal class ExpandedMetal : Part
    {
        // Static properties
        static public double Inset => Framing.THK + GetBendRadius(Framing.THK) + InterferenceClearance;


        // Constructor
        public ExpandedMetal(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Width", "sk:ExpandedMetal", BeltGuardWld.Width - Inset * 2);
            EditDimension("Length", "sk:ExpandedMetal", BeltGuardWld.Length - Inset * 2);
        }


        // Property overrides
        public override bool Enabled => ForcedDraft;
        public override string StaticPartNo => "268M";
        public override Shape RawMaterialShape => Shape.ExpandedMetal;
        public override string SizeOrThickness => "0.1990";
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tY: Framing.THK),
                };
            }
        }
    }
}
