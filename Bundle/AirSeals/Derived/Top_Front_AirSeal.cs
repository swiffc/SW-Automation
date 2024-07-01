using Bundle.Misc;
using Bundle.SideFrame.Derived.Children;
using FileTools.Base;
using ModelTools;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;
using static Tools.ModelTools;

namespace Bundle.AirSeals.Derived
{
    internal class Top_Front_AirSeal : AirSeal
    {
        // Static properties
        protected override double GapToSeal
        {
            get
            {
                return SideFramePart.Depth - (Header61.Y_Location - Header61.Xtop + Tube.OD / 2 + InterferenceClearance);
            }
        }


        // Constructor
        public Top_Front_AirSeal(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }
        public Top_Front_AirSeal(int parentNone) : base(parentNone) { }


        // Property overrides
        public override string StaticPartNo => "1015";
        public override List<PositionData> Position
        {
            get
            {
                if (_pos == null)
                {
                    double zTranslation = TubeLength / 2 - TubeProjection - Header61.TubesheetTHK - Width;
                    double yTranslation = SideFramePart.Depth;

                    _pos = new List<PositionData>
                    {
                        PositionData.Create(tZ: zTranslation, tY:yTranslation)
                    };
                }
                return _pos;
            }
        }



    }
}
