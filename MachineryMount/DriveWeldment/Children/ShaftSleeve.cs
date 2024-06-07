using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;

namespace MachineryMount.DriveWeldment.Children
{
    internal class ShaftSleeve : Part
    {
        // Static properties
        static public double OD => 4.5;


        // Constructor
        public ShaftSleeve(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "Pipe", DriveFrame.TowerHeight - Stringer.Depth - TowerSideC.WebTHK);
        }


        // Property overrides
        public override bool Enabled => ForcedDraft;
        public override string StaticPartNo => "228";
        public override Shape RawMaterialShape => Shape.Pipe;
        public override string SizeOrThickness => "4_Sch40";
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tY: Stringer.Depth),
                };
            }
        }
    }
}
