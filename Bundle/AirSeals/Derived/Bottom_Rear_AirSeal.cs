using Bundle.Misc;
using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;
using static Tools.ModelTools;

namespace Bundle.AirSeals.Derived
{
    internal class Bottom_Rear_AirSeal : AirSeal
    {
        // Constructor
        public Bottom_Rear_AirSeal(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }
        public Bottom_Rear_AirSeal(int parentNone) : base(parentNone) { }


        // Property overrides
        protected override double GapToSeal
        {
            get
            {
                return
                Header62.Y_Location -
                Header62.Xtop -
                RearVerticalPitch._1_2 -
                RearVerticalPitch._2_3 -
                RearVerticalPitch._3_4 -
                RearVerticalPitch._4_5 -
                RearVerticalPitch._5_6 -
                RearVerticalPitch._6_7 -
                RearVerticalPitch._7_8 -
                RearVerticalPitch._8_9 -
                RearVerticalPitch._9_10 -
                Tube.OD / 2 -
                InterferenceClearance;
            }
        }
        protected override double Width
        {
            get
            {
                double unitCenterToTubeSheet = Tube.Length / 2 - TubeProjection - Bundle.LowestFrontHeader.TubesheetTHK - OffsetFromCenter;
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
        public override string StaticPartNo => "1014";
        public override List<PositionData> Position
        {
            get
            {
                double zTranslation = TubeLength / 2 - TubeProjection - Bundle.LowestRearHeader.TubesheetTHK - Width;

                return new List<PositionData>
                {
                    PositionData.Create(tZ: -zTranslation, rX: 180),
                };
            }
        }

    }
}
