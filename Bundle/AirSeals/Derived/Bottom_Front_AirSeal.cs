using Bundle.Misc;
using Bundle.SideFrame.Derived.Children;
using Bundle.TubeSupports;
using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using static Tools.ModelTools;

namespace Bundle.AirSeals.Derived
{
    internal class Bottom_Front_AirSeal : AirSeal
    {
        // Static properties
        protected override double GapToSeal
        {
            get
            {
                return Header61.Y_Location - Header61.Xtop - Tube.AllVerticalPitches - Tube.OD / 2 - InterferenceClearance;
            }
        }


        // Constructor
        public Bottom_Front_AirSeal(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Property overrides
        public override string StaticPartNo => "1013";
        public override List<PositionData> Position
        {
            get
            {
                double zTranslation = TubeLength / 2 - TubeProjection - Bundle.LowestHeader.TubesheetTHK - Width;

                return new List<PositionData>
                {
                    PositionData.Create(tZ: zTranslation, rZ: 180),
                };
            }
        }

    }
}
