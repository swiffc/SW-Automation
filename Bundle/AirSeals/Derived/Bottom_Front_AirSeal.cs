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
        // Constructors
        public Bottom_Front_AirSeal(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }
        public Bottom_Front_AirSeal(int parentNone) : base(parentNone) { }


        // Property overrides
        protected override double Width
        {
            get
            {
                double unitCenterToTubeSheet = Tube.Length / 2 - TubeProjection - Bundle.LowestFrontHeader.TubesheetTHK + OffsetFromCenter;
                double unitCenterToAirSeal = 0;
                switch (Plenum_Design)
                {
                    case Design.Standard:
                        unitCenterToAirSeal = Plenum_Length / 2 + Beam_Depth / 2;
                        break;
                    case Design.Johnson:
                        unitCenterToAirSeal = Plenum_Length / 2 + Johnson_ExtraLength + Beam_FlangeWidth / 2;
                        break;
                    case Design.Legacy:
                        unitCenterToAirSeal = Plenum_Length / 2 + Beam_FlangeWidth / 2;
                        break;
                        throw new Exception("Invalid Plenum Design");
                }

                if (unitCenterToTubeSheet - unitCenterToAirSeal < 2)
                    return HeadersOutsideFrames ? 3 : 2;
                else return unitCenterToTubeSheet - unitCenterToAirSeal;
            }
        }
        protected override double GapToSeal
        {
            get
            {
                return Header61.TubeY - Header61.TubeOddX - Tube.AllFrontVerticalPitches - Tube.OD / 2 - InterferenceClearance;
            }
        }
        public override string StaticPartNo => "1013";
        public override List<PositionData> Position
        {
            get
            {
                if (_pos == null)
                {
                    double zTranslation = TubeLength / 2 - TubeProjection - Bundle.LowestFrontHeader.TubesheetTHK - Width;

                    return new List<PositionData>
                    {
                        PositionData.Create(tZ: zTranslation, rZ: 180),
                    };
                }
                return _pos;
            }
        }
    }
}
