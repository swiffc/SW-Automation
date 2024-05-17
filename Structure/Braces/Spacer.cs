using FileTools.Base;
using ModelTools;
using Structure.Braces.Derived;
using Structure.Columns;
using Structure.Columns.Derived.Children;
using Structure.Columns.Derived.Children.Derived;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Tools.ModelTools;
using static ModelTools.PositionData;
using static FileTools.StaticFileTools;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Structure.Braces
{
    internal class Spacer : Part
    {
        // Constructor
        public Spacer(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("THK", "Plate", Clip_THK);
            EditDimension("Diameter", "sk:Plate", HoleDiameter_Structural);
        }


        // Static methods
        internal static List<PositionData> X_BraceLocations()
        {
            double yTranslation = FieldColumn.Height / 2;
            double zTranslation = Plenum_Length / 2;
            double zRotation = XClip.LocalAngle;

            var pos = new List<PositionData>();
            for (int i = 0; i < Fan_Count + 1; i++)
            {
                if (!Mid_Columns && (i != 0 && i != Fan_Count))
                    continue;

                var positionData = Create(tY: yTranslation, tZ: zTranslation, rZ: zRotation);
                pos.Add(positionData);
                zTranslation -= Mid_Columns ? Plenum_Length / Fan_Count : Plenum_Length;
            }

            return pos;
        }


        // Private methods
        private List<PositionData> LL_EndSpacerLocations()
        {
            // Viewing XY plane 
            // Places spacer on hole nearest column bounds
            double xTranslation = -Plenum_Width / 2 + (Beams_AreRotated ? Beam_Depth / 2 : Beam_FlangeWidth / 2) + ColumnBoundsToHole;
            double yTranslation = ClipHeight;
            double zTranslation = Plenum_Length / 2 + (Beams_AreRotated ? Clip_THK / 2 : 0);
            double zRotation = BraceAngle;

            // Triangle --> places spacer on the slot
            AAS(BraceAngle, out double xNearestHoleToSlot, out double yNearestHoleToSlot, BraceL.HoleToHole);
            xTranslation += xNearestHoleToSlot;
            yTranslation += yNearestHoleToSlot;

            BraceL.IntermediateHoles(out double count, out double spacing);
            // Triangle --> clip slot to brace intermediate hole
            AAS(BraceAngle, out double xSlotToFirstIntermediateHole, out double ySlotToFirstIntermediateHole, spacing);

            var pos = new List<PositionData>();
            double intermediateHoleCount = count - 2;
            for (int i = 0; i < intermediateHoleCount; i++)
            {
                xTranslation += xSlotToFirstIntermediateHole;
                yTranslation += ySlotToFirstIntermediateHole;

                var positionData = Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, rZ: zRotation);
                pos.Add(positionData);

                var positionDataMirror = Create(tX: -xTranslation, tY: yTranslation, tZ: zTranslation, rZ: -zRotation);
                pos.Add(positionDataMirror);
            }

            return pos;
        }
        private List<PositionData> LL_SideSpacerLocations()
        {
            // Viewing YZ plane 
            // Places spacer on hole nearest column bounds
            double xTranslation = -Plenum_Width / 2;
            double yTranslation = ClipHeight;
            double zTranslation = Plenum_Length / 2 - (Beams_AreRotated ? Beam_FlangeWidth / 2 : Beam_Depth / 2) - ColumnBoundsToHole;
            double xRotation = 90;
            double zRotation = 90;
            double yRotation = BraceAngle;

            // Triangle --> places spacer on the slot
            AAS(BraceAngle, out double zNearestHoleToSlot, out double yNearestHoleToSlot, BraceL.HoleToHole);
            zTranslation -= zNearestHoleToSlot;
            yTranslation += yNearestHoleToSlot;

            BraceL.IntermediateHoles(out double count, out double spacing);
            // Triangle --> clip slot to brace intermediate hole
            AAS(BraceAngle, out double zSlotToFirstIntermediateHole, out double ySlotToFirstIntermediateHole, spacing);

            var pos = new List<PositionData>();
            double intermediateHoleCount = count - 2;
            for (int i = 0; i < intermediateHoleCount; i++)
            {
                zTranslation -= zSlotToFirstIntermediateHole;
                yTranslation += ySlotToFirstIntermediateHole;

                var positionData = Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, rX: xRotation, rY: yRotation, rZ: zRotation);
                pos.Add(positionData);

                var positionDataMirror = Create(tX: -xTranslation, tY: yTranslation, tZ: zTranslation, rX: xRotation, rY: yRotation, rZ: zRotation);
                pos.Add(positionDataMirror);
            }

            return pos;
        }
        private List<PositionData> LL_OppositeEndSpacerLocations(List<PositionData> list)
        {
            var newList = new List<PositionData>();
            for (int i = 0; i < list.Count; i++)
            {
                PositionData positionData = list[i];
                positionData.TranslationZ *= -1;
                newList.Add(positionData);
            }
            return newList;
        }
        private List<PositionData> RotatePositionsAroundY(List<PositionData> positions, double angleDegrees)
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
                    rY: position.RotationY,
                    rZ: position.RotationZ + angleDegrees
                );

                rotatedPositions.Add(newPosition);
            }

            return rotatedPositions;
        }



        // Property overrides
        public override bool Enabled => new[] { "X", "TX", "LL" }.Contains(BraceType);
        public override string StaticPartNo => "132S";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => Clip_THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                var pos = new List<PositionData>();

                if (BraceType.Contains("X"))
                {
                    return X_BraceLocations();
                }
                else if (BraceType.Contains("L"))
                {
                    var endSpacers = LL_EndSpacerLocations();
                    var sideSpacers = LL_SideSpacerLocations();

                    var oppositeEndSpacers = LL_OppositeEndSpacerLocations(endSpacers);
                    var oppositeSideSpacers = RotatePositionsAroundY(sideSpacers, 180);

                    pos.AddRange(endSpacers);
                    pos.AddRange(sideSpacers);
                    pos.AddRange(oppositeEndSpacers);
                    pos.AddRange(oppositeSideSpacers);

                    if (Mid_Columns)
                    {
                        var mids = BraceL.MidColumnElements(sideSpacers, oppositeSideSpacers, endSpacers);
                        pos.AddRange(mids);
                    }
                }
                return pos;
            }
        }
    }
}
