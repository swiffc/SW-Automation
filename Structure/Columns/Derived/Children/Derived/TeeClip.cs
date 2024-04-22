using FileTools.Base;
using ModelTools;
using Structure.Braces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static System.Windows.Forms.VisualStyles.VisualStyleElement.TextBox;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Structure.Columns.Derived.Children.Derived
{
    internal class TeeClip : Clip
    {
        // Static properties
        static public double Landing
        {
            get
            {
                double columnBounds = Beams_AreRotated ? Beam_WebTHK : Beam_Depth;
                double landing = (BasePlate.LocalLength - columnBounds) / 2;

                if (landing <= 0)
                    return 1;
                else
                    return landing;
            }
        }
        static public double OffsetFromColumnCenter => Beams_AreRotated ? Beam_WebTHK / 2 : Beam_FlangeWidth / 2;
        static public double ColumnBoundToNearestHole => Beams_AreRotated ? Beam_FlangeWidth / 2 - Beam_WebTHK / 2 + ColumnBoundsToHole : ColumnBoundsToHole;


        // Constructor
        public TeeClip(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            base.Dimensions();

            EditDimension("Offset", "sk:Plate", OffsetFromColumnCenter);
            EditDimension("ColumnBoundsToHole", "sk:Plate", ColumnBoundToNearestHole);
            EditDimension("Landing", "sk:Plate", Landing);
            EditDimension("FlangeGage", "sk:Plate", WT_FlangeGage);
            EditDimension("Angle", "sk:Plate", BraceAngle);

        }


        // Property overrides
        public override bool Enabled => new[] { "T", "TX" }.Contains(BraceType);
        public override string StaticPartNo => "104T";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => Clip_THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                var pos = new List<PositionData>();

                if (ParentSubAssembly.StaticPartNo != "111")
                {
                    double rotation = 90;
                    if (ParentSubAssembly.StaticPartNo == "101")
                        rotation *= -1;
                    pos.Add(PositionData.Create(tY: BasePlate.THK, rY: rotation));
                }

                return pos;
            }
        }
    }
}
