using FileTools.Base;
using ModelTools;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using static FileTools.StaticFileTools;


namespace Structure.Columns.Derived.Children
{
    internal class Beam : Part
    {
        // Static properties
        static internal string Size { get; set; } = "W8x28";
        static internal bool Rotate { get; set; } = false;
        static internal double Length => Column.Height - BasePlate.THK - EndPlate.THK;


        // Constructor
        public Beam(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Path", Length);
        }


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => "101P";
        public override Shape RawMaterialShape => Shape.Beam;
        public override string SizeOrThickness => Size;
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tY: BasePlate.THK, rY:Rotate ? 90 : 0)
                };
            }
        }
    }
}
