using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.AccessControl;
using System.Text;
using System.Threading.Tasks;
using static FileTools.SharedProperties;

namespace Structure.Columns.Derived.Children
{
    internal class WebClip : Part
    {
        // Static properties
        static public double THK { get; set; } = 0.25;


        // Constructor
        public WebClip(SubAssembly parentSubAssembly) : base(parentSubAssembly)
        {
            parent = parentSubAssembly;
        }


        // Property overrides
        public override bool Enabled => new[] { "L", "LL" }.Contains(BraceType);
        public override string StaticPartNo => "104W";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                var pos = new List<PositionData>();

                if (parent.StaticPartNo != "106")
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
