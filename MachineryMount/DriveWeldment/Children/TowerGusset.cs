using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using static FileTools.CommonData.CommonData;
using static Tools.ModelTools;

namespace MachineryMount.DriveWeldment.Children
{
    internal class TowerGusset : Part
    {
        // Constructor
        public TowerGusset(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Plate", (DriveFrame.Width - ShaftSleeve.OD)/2 - WeldClearance * 2);
        }


        // Property overrides
        public override bool Enabled => ForcedDraft;
        public override string StaticPartNo => "229";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => "0.25";
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tY: Stringer.Depth, tZ: ShaftSleeve.OD/2 + WeldClearance),
                    PositionData.Create(tY: Stringer.Depth, tZ: -(ShaftSleeve.OD/2 + WeldClearance), rY: 180),
                };
            }
        }
    }
}
