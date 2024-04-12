using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Structure.Columns.Derived.Children
{
    internal class WebClip : Part
    {
        static public double THK { get; set; } = 0.25;
        public WebClip(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }

        public override bool Enabled => true;

        public override string StaticPartNo => "104W";

        public override Shape RawMaterialShape => Shape.Plate;

        public override string SizeOrThickness => THK.ToString();

        public override List<PositionData> Position => throw new NotImplementedException();
    }
}
