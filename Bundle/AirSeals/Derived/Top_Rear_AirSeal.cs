using Bundle.Misc;
using Bundle.SideFrame.Derived.Children;
using FileTools.Base;
using ModelTools;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;
using static Tools.ModelTools;

namespace Bundle.AirSeals.Derived
{
    internal class Top_Rear_AirSeal : AirSeal
    {
        // Static properties
        protected override double GapToSeal
        {
            get
            {
                return SideFramePart.Depth - (Header62.Y_Location - Header62.Xtop + Tube.OD / 2 + InterferenceClearance);
            }
        }


        // Constructor
        public Top_Rear_AirSeal(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Property overrides
        public override string StaticPartNo => "1016";
        public override List<PositionData> Position
        {
            get
            {
                double zTranslation = TubeLength / 2 - TubeProjection - Bundle.LowestHeader.TubesheetTHK - Width;
                double yTranslation = SideFramePart.Depth;

                return new List<PositionData>
                {
                    PositionData.Create(tZ: -zTranslation, tY: yTranslation, rY: 180),
                };
            }
        }

    }
}
