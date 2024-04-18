using FileTools.Base;
using ModelTools;
using Structure.Columns;
using Structure.Columns.Derived.Children;
using Structure.Columns.Derived.Children.Derived;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Tools.ModelTools;

namespace Structure.Braces
{
    internal class BraceX : Part
    {
        // Static properties
        static public double LocalLength
        {
            get
            {
                // Viewing XY plane
                // Triangle --> [work point to work point in Y direction] to [work point to work point in X direction]
                AAS(out double _, Beam.LocalLength, Width, out double workPointToDiagonalWorkPoint);

                // Triangle --> [work point] to [nearest hole]
                double xWorkPointToNearestHole = XClip.ColumnBoundToNearestHole + XClip.OffsetFromColumnCenter;
                AAS(XClip.LocalAngle, out _, xWorkPointToNearestHole, out double diagonalFromWorkPointToNearestHole);

                double length = workPointToDiagonalWorkPoint - diagonalFromWorkPointToNearestHole * 2 + HoleToEnd * 2;

                return length;
            }
        }
        static public double HoleToEnd => 1.125;


        // Constructor
        public BraceX(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "L", LocalLength);
        }


        // Property overrides
        public override bool Enabled => new[] { "X", "TX" }.Contains(BraceType);
        public override string StaticPartNo => "131X";
        public override Shape RawMaterialShape => Shape.Angle;
        public override string SizeOrThickness => "3x3x0.25";
        public override List<PositionData> Position
        {
            get
            {
                var pos = new List<PositionData>();

                var braces1 = Spacer.X_BraceLocations();
                for (int i = 0; i < braces1.Count; i++)
                {
                    PositionData brace = braces1[i];
                    brace.TranslationZ -= Clip.THK/2;
                    braces1[i] = brace;
                }
                pos.AddRange(braces1);


                var braces2 = Spacer.X_BraceLocations();
                for (int i = 0; i < braces2.Count; i++)
                {
                    PositionData brace = braces2[i];
                    brace.TranslationZ += Clip.THK/2;
                    brace.RotationY = 180;
                    brace.RotationZ *= -1;
                    braces2[i] = brace;
                }
                pos.AddRange(braces2);

                return pos;
            }
        }
    }
}
