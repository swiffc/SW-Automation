using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FileTools.Base;
using ModelTools;

namespace MachineryMount.Drive_Mount.Plate
{
    class ShaftPanel : Part
    {
        public ShaftPanel(SubAssembly parentSubAssembly) : base(parentSubAssembly)
        {

        }

        public override bool Enabled => throw new NotImplementedException();

        public override string StaticPartNo => throw new NotImplementedException();

        public override Shape RawMaterialShape => throw new NotImplementedException();

        public override string SizeOrThickness => throw new NotImplementedException();

        public override List<PositionData> Position => throw new NotImplementedException();
    }
}
