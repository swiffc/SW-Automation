using FileTools.Base;
using ModelTools;
using Structure.Braces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Tools.ModelTools;

namespace Structure.Columns.Derived.Children.Derived
{
    internal class XClip : Clip
    {
        // Static properties
        static public double OffsetFromColumnCenter => Beam.IsRotated ? Beam.FlangeWidth / 2 : Beam.WebTHK / 2;
        static public double ColumnBoundToNearestHole => Beam.IsRotated ? ColumnBoundsToHole : Beam.FlangeWidth / 2 - Beam.WebTHK / 2 + ColumnBoundsToHole;
        static public double LocalAngle
        {
            get
            {
                // Viewing XY plane
                // Triangle --> [work point to work point in Y direction] to [work point to work point in X direction]
                AAS(out double angle, Beam.LocalLength, Width, out double _);
                return angle;
            }
        }


        // Constructor
        public XClip(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            base.Dimensions();

            EditDimension("Offset", "sk:Plate", OffsetFromColumnCenter);
            EditDimension("HoleToColumnBounds", "sk:Plate", ColumnBoundToNearestHole);
            EditDimension("THK", "Plate", THK);
            EditDimension("Angle", "sk:Plate", LocalAngle);
        }


        // Property overrides
        public override bool Enabled => new[] { "X", "TX" }.Contains(BraceType);
        public override string StaticPartNo => "104X";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                var pos = new List<PositionData>
                {
                    PositionData.Create(tY: BasePlate.THK),
                    PositionData.Create(tY: FieldColumn.Height - EndPlate.THK, rX: 180)
                };

                return pos;
            }
        }
    }
}
