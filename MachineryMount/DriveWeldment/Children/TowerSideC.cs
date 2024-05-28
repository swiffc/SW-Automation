using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;

namespace MachineryMount.DriveWeldment.Children
{
    internal class TowerSideC : Part
    {
        // Static properties
        static public string Size
        {
            get
            {
                return FanShaft_Diameter <= 2.25 ? "C8x11.5" : "C10x15.3";
            }
        }


        // Constructor
        public TowerSideC(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Path", DriveFrame.TowerHeight - Stringer.Depth);
        }
        protected override void Features()
        {
            EditFeature_StructuralMemberSize(Size);
        }


        // Property overrides
        public override bool Enabled => ForcedDraft;
        public override string StaticPartNo => "226P";
        public override Shape RawMaterialShape => Shape.Channel;
        public override string SizeOrThickness => Size;
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tY: Stringer.Depth, tZ: DriveFrame.Width/2),
                    PositionData.Create(tY: Stringer.Depth, tZ: -DriveFrame.Width/2, rY: 180)
                };
            }
        }
    }
}
