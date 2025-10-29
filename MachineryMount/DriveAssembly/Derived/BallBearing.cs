using FileTools.Base;
using MachineryMount.DriveWeldment;
using MachineryMount.DriveWeldment.Children;
using ModelTools;
using System.Collections.Generic;

namespace MachineryMount.DriveAssembly.Derived
{
    internal class BallBearing : Bearing
    {
        // Constructor
        public BallBearing(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => SelectBallBearing();
        public override string PartNo => StaticPartNo;
        public override string FilePath => $@"C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\templates\common_library\Bearings\{StaticPartNo}.SLDPRT";
        public override List<PositionData> Position
        {
            get
            {
                var pos = new List<PositionData>
                {
                    PositionData.Create(tY: Stringer.Depth - GuardPlate.THK, rX: -90),
                };

                if (!RollerBearingRequired)
                    pos.Add(PositionData.Create(tY: DriveFrame.TowerHeight, rX: 90));

                return pos; 
            }
        }
    }
}
