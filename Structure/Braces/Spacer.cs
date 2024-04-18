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

namespace Structure.Braces
{
    internal class Spacer : Part
    {
        // Constructor
        public Spacer(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("THK", "Plate", Clip.THK);
        }


        // Static methods
        internal static List<PositionData> X_BraceLocations()
        {
            double yTranslation = FieldColumn.Height / 2;
            double zTranslation = Length / 2;
            double zRotation = XClip.LocalAngle;

            var pos = new List<PositionData>();
            for (int i = 0; i < FanCount + 1; i++)
            {
                if (!MidColumns && (i != 0 && i != FanCount))
                    continue;

                var positionData = Create(tY: yTranslation, tZ: zTranslation, rZ: zRotation);
                pos.Add(positionData);
                zTranslation -= MidColumns ? Length / FanCount : Length;
            }

            return pos;
        }


        // Private methods
        private List<PositionData> LL_EndSpacerLocations()
        {
            // Viewing XY plane 
            // Places spacer on hole nearest column bounds
            double xTranslation = -Width / 2 + (Beam.IsRotated ? Beam.Depth / 2 : Beam.FlangeWidth / 2) + Clip.ColumnBoundsToHole;
            double yTranslation = ClipHeight;
            double zTranslation = Length / 2;
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
            double xTranslation = -Width / 2;
            double yTranslation = ClipHeight;
            double zTranslation = Length / 2 - (Beam.IsRotated ? Beam.FlangeWidth / 2 : Beam.Depth / 2) - Clip.ColumnBoundsToHole;
            double yRotation = 270;
            double zRotation = 0; 

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

                var positionData = Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, rY: yRotation, rZ: zRotation);
                pos.Add(positionData);

                var positionDataMirror = Create(tX: -xTranslation, tY: yTranslation, tZ: zTranslation, rY: yRotation, rZ: -zRotation);
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



        // Property overrides
        public override bool Enabled => new[] { "X", "TX", "LL" }.Contains(BraceType);
        public override string StaticPartNo => "132S";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => Clip.THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                if (BraceType.Contains("X"))
                    return X_BraceLocations();
                else
                {
                    var pos = new List<PositionData>();

                    var endSpacers = LL_EndSpacerLocations();
                    var sideSpacers = LL_SideSpacerLocations();

                    var oppositeEndSpacers = LL_OppositeEndSpacerLocations(endSpacers);
                    var oppositeSideSpacers = BraceL.RotatePositionsAroundY(sideSpacers, 180);

                    pos.AddRange(endSpacers);
                    pos.AddRange(sideSpacers);
                    pos.AddRange(oppositeEndSpacers);
                    pos.AddRange(oppositeSideSpacers);

                    if (MidColumns)
                    {
                        var mids = BraceL.MidColumnElements(sideSpacers, oppositeSideSpacers, endSpacers);
                        pos.AddRange(mids);
                    }

                    return pos;
                }
            }
        }
    }
}
