using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FileTools.Base;
using ModelTools;

namespace MachineryMount.Drive_Mount.Angle
{
    class AdjustmentAngle : Part
    {
        public AdjustmentAngle(SubAssembly parentSubAssembly) : base(parentSubAssembly)
        {
        }

        public override bool Enabled => true;

        public override string StaticPartNo => "238";

        public override Shape RawMaterialShape => Shape.Channel;

        public override string SizeOrThickness => throw new NotImplementedException();

        public override List<PositionData> Position => throw new NotImplementedException();
    }
}
