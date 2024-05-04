using FileTools.Base;
using ModelTools;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Structure.Columns.Derived.Children
{
    internal class EndPlate : Part
    {
        // Static properties
        static public double THK => 0.5;
        static public double LocalWidth => Beams_AreRotated ? Beam_Depth : Beam_FlangeWidth;
        static public double LocalLength => Beams_AreRotated ? Beam_FlangeWidth : Beam_Depth;
        static public double HoleDiameter { get; set; } = 0.8125;
        static public double HoleToEdge => 1.25;


        // Constructor
        public EndPlate(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Width", "sk:Plate", LocalWidth);
            EditDimension("Length", "sk:Plate", LocalLength);
            EditDimension("THK", "Plate", THK);

            EditDimension("Diameter", "sk:Hole", HoleDiameter);
            EditDimension("WidthSpacing", "sk:Hole", LocalWidth - HoleToEdge * 2);
            EditDimension("LengthSpacing", "sk:Hole", LocalLength - HoleToEdge * 2);
        }


        // Property overrides
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
                    PositionData.Create(tY: FieldColumn.Height)
                };
            }
        }
    }
}
