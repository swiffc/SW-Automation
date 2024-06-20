using Bundle.Misc;
using FileTools.Base;
using ModelTools;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;
using static Tools.ModelTools;

namespace Bundle.AirSeals.Derived
{
    internal class Bottom_Rear_AirSeal : AirSeal
    {
        // Static properties
        protected override double GapToSeal
        {
            get
            {
                // Bottom tube location on inlet side
                double yBottomTubeAtFrontHeader = Header61.Y_Location - Header61.Xtop - Tube.AllVerticalPitches - Tube.OD / 2 - InterferenceClearance;

                // 
                double[] verticalPitches = new double[]
                {                       // 1
                    VerticalPitch._1_2, // 2
                    VerticalPitch._2_3, // 3
                    VerticalPitch._3_4, // 4
                    VerticalPitch._4_5, // 5
                    VerticalPitch._5_6, // 6
                    VerticalPitch._6_7, // 7
                    VerticalPitch._7_8, // 8
                    VerticalPitch._8_9, // 9
                    VerticalPitch._9_10,// 10
                };
                double[] slopesPerFoot = new double[]
                {
                    SlopePerFoot.Row1, // 1
                    SlopePerFoot.Row2, // 2
                    SlopePerFoot.Row3, // 3
                    SlopePerFoot.Row4, // 4
                    SlopePerFoot.Row5, // 5
                    SlopePerFoot.Row6, // 6
                    SlopePerFoot.Row7, // 7
                    SlopePerFoot.Row8, // 8
                    SlopePerFoot.Row9, // 9
                    SlopePerFoot.Row10 // 10
                };
                double slopeOfLastPass = 0;

                for (int i = 0; i < verticalPitches.Length; i++)
                {
                    if (verticalPitches[i] == 0)
                    {
                        slopeOfLastPass = slopesPerFoot[i];
                    }
                }

                return yBottomTubeAtFrontHeader - slopeOfLastPass * Tube.Length / 12;
            }
        }


        // Constructor
        public Bottom_Rear_AirSeal(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Property overrides
        public override string StaticPartNo => "1014";
        public override List<PositionData> Position
        {
            get
            {
                double zTranslation = TubeLength / 2 - TubeProjection - Bundle.LowestHeader.TubesheetTHK - Width;

                return new List<PositionData>
                {
                    PositionData.Create(tZ: -zTranslation, rX: 180),
                };
            }
        }

    }
}
