using FileTools.Base;
using ModelTools;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;


namespace Structure.Columns.Derived.Children
{
    internal class Beam : Part
    {
        // Static properties


        static internal double LocalLength => FieldColumn.Height - BasePlate_THK - EndPlate.THK;


        // Constructor
        public Beam(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "Beam", LocalLength);

            EditDimension("Depth", "sk:Beam", Beam_Depth);
            EditDimension("WebTHK", "sk:Beam", Beam_WebTHK);
            EditDimension("FlangeWidth", "sk:Beam", Beam_FlangeWidth);
            EditDimension("FlangeTHK", "sk:Beam", Beam_FlangeTHK);
            EditDimension("K", "sk:Beam", Beam_K);
            EditDimension("K1", "sk:Beam", Beam_K1);
        }


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => "101P";
        public override Shape RawMaterialShape => Shape.Beam;
        public override string SizeOrThickness => Beam_Size;
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tY: BasePlate_THK, rY:Beams_AreRotated ? 90 : 0)
                };
            }
        }


        // Private properties

    }
}
