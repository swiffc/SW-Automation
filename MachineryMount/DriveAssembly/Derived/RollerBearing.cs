using FileTools.Base;
using MachineryMount.DriveWeldment;
using MachineryMount.DriveWeldment.Children;
using ModelTools;
using System.Collections.Generic;

namespace MachineryMount.DriveAssembly.Derived
{
    internal class RollerBearing : Bearing
    {
        // Constructor
        public RollerBearing(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Property overrides
        public override bool Enabled => RollerBearingRequired;
        public override string StaticPartNo => SelectRollerBearing();
        public override string PartNo => StaticPartNo;
        public override string FilePath => $@"C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\templates\common_library\Bearings\{StaticPartNo}.SLDPRT";
        public override List<PositionData> Position
        {
            get
            {
                var pos = new List<PositionData>();

                if (RollerBearingRequired)
                {
                    pos.Add(PositionData.Create(tY: DriveFrame.TowerHeight, rX: 90));
                }

                return pos;
            }
        }
    }
}
