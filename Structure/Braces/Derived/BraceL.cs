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
using System;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;
using FileTools;

namespace Structure.Braces.Derived
{
    internal class BraceL : AngleBrace
    {
        // Static properties
        static public double LocalLength
        {
            get
            {
                return KneeBraceLength();
            }
        }
        static public double HoleToEnd => CommonData.HoleToEnd;
        static public double HoleToHole => 2.5;


        // Constructor
        public BraceL(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            base.Dimensions();

            IntermediateHoles(out double count, out double spacing);
            EditDimension("Count", "sk:Hole", count);
            EditDimension("Spacing", "sk:Hole", spacing);
        }


        // Static methods
        internal static void IntermediateHoles(out double count, out double spacing)
        {
            double span = LocalLength - HoleToEnd * 2 - HoleToHole * 2;
            HolePattern(span, out count, out spacing);
            count = BraceType == "LL" ? count : 2;
            spacing = BraceType == "LL" ? spacing : span;
        }
        internal static List<PositionData> RotatePositionsAroundY(List<PositionData> positions, double angleDegrees)
        {
            double angleRadians = angleDegrees * Math.PI / 180;
            List<PositionData> rotatedPositions = new List<PositionData>();

            foreach (var position in positions)
            {
                double cosTheta = Math.Cos(angleRadians);
                double sinTheta = Math.Sin(angleRadians);

                double newX = position.TranslationX * cosTheta - position.TranslationZ * sinTheta;
                double newZ = position.TranslationX * sinTheta + position.TranslationZ * cosTheta;

                PositionData newPosition = PositionData.Create(
                    tX: newX,
                    tY: position.TranslationY,
                    tZ: newZ,
                    rX: position.RotationX,
                    rY: position.RotationY + angleDegrees,
                    rZ: position.RotationZ
                );

                rotatedPositions.Add(newPosition);
            }

            return rotatedPositions;
        }
        internal static List<PositionData> MidColumnElements(List<PositionData> sidePositions, List<PositionData> oppositeSide, List<PositionData> endPositions)
        {
            double columnToColumn = Length / FanCount;

            var sidePattern = PositionPatternZ(sidePositions, columnToColumn);
            var oppositeSidePattern = PositionPatternZ(oppositeSide, -columnToColumn);
            var endPattern = PositionPatternZ(endPositions, columnToColumn);

            var pos = new List<PositionData>();
            pos.AddRange(sidePattern);
            pos.AddRange(oppositeSidePattern);
            pos.AddRange(endPattern);

            return pos;
        }



        // Private methods
        private List<PositionData> SidePositions(out double horz, out double yTranslation, out double xRotation)
        {
            // Viewing YZ plane
            double xTranslation = -Width / 2 + (Beams_AreRotated ? Clip_THK / 2 : 0);
            yTranslation = ClipHeight;
            double zTranslation;
            if (Beams_AreRotated)
            {
                zTranslation = Length / 2 - Beam_FlangeWidth / 2 - ColumnBoundsToHole;
            }
            else
            {
                zTranslation = Length / 2 - FlangeClip.xzTranslation;
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

            var pos = new List<PositionData>
            {
                side_101,
                side_106
            };

            if (BraceType == "LL")
            {
                var side_101LL = side_101;
                side_101LL.TranslationX -= Clip_THK;
                side_101LL.RotationY += 180;
                side_101LL.RotationX += 90 + BraceAngle;
                pos.Add(side_101LL);

                var side_106LL = side_106;
                side_106LL.TranslationX += Clip_THK;
                side_106LL.RotationY += 180;
                side_106LL.RotationX -= 90 + BraceAngle;
                pos.Add(side_106LL);
            }

            return pos;
        }
        private List<PositionData> EndPositions(double horz, double yTranslation, double xRotation)
        {
            // Viewing XY plane

            double xTranslation;
            double zTranslation;
            string test = PlenumDesign.ToString();
            if (PlenumDesign == Design.Standard)
            {
                xTranslation = -Width / 2 + Beam_FlangeWidth / 2 + ColumnBoundsToHole + horz;
                zTranslation = Length / 2 - Clip_THK / 2;
            }
            else
            {
                xTranslation = -Width / 2 + Beam_FlangeWidth / 2 + ColumnBoundsToHole + horz;
                zTranslation = Length / 2;
            }


            var end_101 = PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, rX: -xRotation, rY: -90);

            var end_106 = end_101;
            end_106.TranslationX = -xTranslation;
            end_106.RotationX = 90 + BraceAngle;

            var pos = new List<PositionData>
            {
                end_101,
                end_106 ,
            };

            if (BraceType == "LL")
            {
                var end_101LL = end_101;
                end_101LL.TranslationZ += Clip_THK;
                end_101LL.RotationY += 180;
                end_101LL.RotationX -= 90 + BraceAngle;
                pos.Add(end_101LL);

                var end_106LL = end_106;
                end_106LL.TranslationZ += Clip_THK;
                end_106LL.RotationY += 180;
                end_106LL.RotationX += 90 + BraceAngle;
                pos.Add(end_106LL);
            }

            return pos;
        }
        private static List<PositionData> PositionPatternZ(List<PositionData> referencePosition, double increment)
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
        public override string StaticPartNo => "131L";
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
                    var mids = MidColumnElements(sidePositions, oppositeSide, endPositions);
                    pos.AddRange(mids);
                }

                return pos;
            }
        }
    }
}
