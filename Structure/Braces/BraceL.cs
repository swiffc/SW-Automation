using FileTools.Base;
using ModelTools;
using Structure.Columns.Derived.Children;
using Structure.Columns.Derived.Children.Derived;
using System.Collections.Generic;
using static Tools.ModelTools;
using static FileTools.StaticFileTools;
using static System.Net.Mime.MediaTypeNames;
using System.Linq;
using System.Security.AccessControl;

namespace Structure.Braces.Derived
{
    internal class BraceL : Part
    {
        // Static properties
        static public string Size { get; set; } = "3x3x0.25";
        static public double LocalLength
        {
            get
            {
                double BottomOfPlenumToHole = 2.5;
                double yPlenumClipHole = ColumnHeight - PlenumDepth - BottomOfPlenumToHole;
                double yStructureClipHole = ClipHeight;

                // Triangle
                double horz;
                double vert = yPlenumClipHole - yStructureClipHole;
                double diag;
                double angle = BraceAngle;

                AAS(angle, out horz, vert, out diag);
                double plenumClipTranslation = horz;
                double braceLength = diag + HoleToEnd * 2;

                return braceLength;
            }
        }
        static public double HoleToEnd => 1.125;


        // Constructor
        public BraceL(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "L", LocalLength);
        }


        // Private methods
        private List<PositionData> SidePositions(out double horz, out double yTranslation, out double xRotation)
        {
            // Viewing YZ plane
            double xTranslation = -Width / 2 + Clip.THK / 2;
            yTranslation = ClipHeight;
            double zTranslation;
            if (Beam.IsRotated)
            {
                zTranslation = Length / 2 - Beam.FlangeWidth / 2 - Clip.ColumnBoundsToHole;
            }
            else
            {
                zTranslation = Length / 2 - FlangeClip.zTranslation;
            }
            xRotation = 180 - (90 - BraceAngle);

            // Triangle : brace origin @ clip hole --> brace hole at clip hole
            double vert;
            double diag = LocalLength / 2 - HoleToEnd;

            AAS(BraceAngle, out horz, out vert, diag);
            yTranslation += vert;
            zTranslation -= horz;

            var side_101 = PositionData.Create(tX: xTranslation, rX: xRotation, tY: yTranslation, tZ: zTranslation);

            var side_106 = side_101;
            side_106.TranslationX = -xTranslation;
            side_106.RotationY = 180;
            side_106.RotationX = 180 + (90 - BraceAngle);

            return new List<PositionData>
            {
                side_101,
                side_106
            };
        }
        private List<PositionData> EndPositions(double horz, double yTranslation, double xRotation)
        {
            // Viewing XY plane
            double xTranslation = -Width / 2 + Beam.FlangeWidth / 2 + Clip.ColumnBoundsToHole + horz;
            double zTranslation = Length / 2 - Clip.THK / 2;

            var end_101 = PositionData.Create(tX: xTranslation, rX: -xRotation, tY: yTranslation, rY: -90, tZ: zTranslation);

            var end_106 = end_101;
            end_106.TranslationX = -xTranslation;
            end_106.RotationX = 90 + BraceAngle;

            return new List<PositionData>
            {
                end_101,
                end_106 ,
            };
        }
        private List<PositionData> PositionPatternZ(List<PositionData> referencePosition, double increment)
        {
            var pos = new List<PositionData>();

            for (int i = 1; i < FanCount; i++)
            {
                foreach (PositionData originalPos in referencePosition)
                {
                    PositionData modifiedPos = new PositionData(
                        originalPos.TranslationX,
                        originalPos.TranslationY,
                        originalPos.TranslationZ - increment * i,
                        originalPos.RotationX,
                        originalPos.RotationY,
                        originalPos.RotationZ
                    );

                    pos.Add(modifiedPos);
                }
            }

            return pos;
        }


        // Property overrides
        public override bool Enabled => new[] { "L", "LL" }.Contains(BraceType);
        public override string StaticPartNo => "131";
        public override Shape RawMaterialShape => Shape.Angle;
        public override string SizeOrThickness => Size;
        public override List<PositionData> Position
        {
            get
            {
                var sidePositions = SidePositions(out double horz, out double yTranslation, out double xRotation);
                var endPositions = EndPositions(horz, yTranslation, xRotation);

                var oppositeSide = RotatePositionsAroundY(sidePositions, 180);
                var oppositeEnd = RotatePositionsAroundY(endPositions, 180);

                var pos = new List<PositionData>();
                pos.AddRange(sidePositions);
                pos.AddRange(endPositions);
                pos.AddRange(oppositeSide);
                pos.AddRange(oppositeEnd);

                if (MidColumns)
                {
                    double columnToColumn = Length / FanCount;

                    var sidePattern = PositionPatternZ(sidePositions, columnToColumn);
                    var oppositeSidePattern = PositionPatternZ(oppositeSide, -columnToColumn);
                    var endPattern = PositionPatternZ(endPositions, columnToColumn);

                    pos.AddRange(sidePattern);
                    pos.AddRange(oppositeSidePattern);
                    pos.AddRange(endPattern);
                }

                return pos;
            }
        }
    }
}
