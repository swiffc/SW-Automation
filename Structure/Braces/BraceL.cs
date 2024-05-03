using FileTools.Base;
using ModelTools;
using System.Collections.Generic;

namespace Structure.Braces
{
    internal class BraceL : Part
    {
        // Static properties
        public static string Size { get; set; } = "3x3x0.25";


        // Constructor
        public BraceL(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Property overrides
        public override bool Enabled => false;
        public override string StaticPartNo => "131";
        public override Shape RawMaterialShape => Shape.Angle;
        public override string SizeOrThickness => Size;
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create()
                };
            }
        }


    }
}
