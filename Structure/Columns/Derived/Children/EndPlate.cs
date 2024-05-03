using FileTools.Base;
using ModelTools;
using System.Collections.Generic;

namespace Structure.Columns.Derived.Children
{
    internal class EndPlate : Part
    {
        static public double THK => 0.5;
        public EndPlate(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }
        public override bool Enabled => true;
        public override string StaticPartNo => "103S";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tY: Column.Height)
                };
            }
        }
    }
}
