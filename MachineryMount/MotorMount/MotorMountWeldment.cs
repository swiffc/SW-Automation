using FileTools.Base;
using MachineryMount.DriveWeldment;
using MachineryMount.DriveWeldment.Children;
using MachineryMount.Mechanicals;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;

namespace MachineryMount.MotorMount
{
    internal class MotorMountWeldment : SubAssembly
    {
        // Constructor
        public MotorMountWeldment(SW_Assembly parentAssembly) : base(parentAssembly) { }


        // Property overrides
        public override bool Enabled => ForcedDraft;
        public override string StaticPartNo => "234";
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tX: -MachineryMount.CenterToCenter, tY: Stringer.Depth, rY: -90)
                };
            }
        }
    }
}
