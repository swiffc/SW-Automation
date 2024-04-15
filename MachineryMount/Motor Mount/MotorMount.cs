using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FileTools.Base;
using ModelTools;

namespace MachineryMount.Drive_Mount
{
    class MotorMount : SubAssembly
    {
        public MotorMount(SW_Assembly parentAssembly) : base(parentAssembly)
        {
        }

        public override bool Enabled => throw new NotImplementedException();

        public override string StaticPartNo => throw new NotImplementedException();

        public override List<PositionData> Position => throw new NotImplementedException();
    }
}
