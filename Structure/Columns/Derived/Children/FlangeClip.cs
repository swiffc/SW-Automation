using FileTools.Base;
using ModelTools;
using System.Collections.Generic;

namespace Structure.Columns.Derived.Children
{
    internal class FlangeClip : Part
    {
        // Static properties
        static public double THK { get; set; } = 0.25;


        // Constructor
        public FlangeClip(SubAssembly parentSubAssembly) : base(parentSubAssembly)
        {
            parent = parentSubAssembly;
        }


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => "104F";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                var pos = new List<PositionData>();

                if (parent.StaticPartNo == "106")
                {
                    pos.Add(Column.WebClipPos);
                }

                return pos;
            }
        }


        // Private properties
        SubAssembly parent;
    }
}
