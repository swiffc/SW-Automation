using FileTools.Base;
using ModelTools;
using Structure.Columns.Derived.Children;
using Structure.Columns.Derived.Children.Derived;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Tools.ModelTools;
using static Plenum.Plenum;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Structure.Braces
{
    internal class BraceT : Part
    {
        // Static properties



        // Constructor
        public BraceT(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            CalculateLengthAndPositionData();
            EditDimension("Length", "T", (double)_length);
            EditDimension("Diameter", "sk:Hole", HoleDiameter_Structural);

            EditDimension("Depth", "sk:T", WT_Depth);
            EditDimension("StemTHK", "sk:T", WT_StemTHK);
            EditDimension("FlangeWidth", "sk:T", WT_FlangeWidth);
            EditDimension("FlangeTHK", "sk:T", WT_FlangeTHK);
            EditDimension("K", "sk:T", WT_K);
            EditDimension("K1", "sk:T", WT_K1);
            EditDimension("FlangeGage", "sk:Hole", WT_FlangeGage);
        }


        // Private properties
        private void CalculateLengthAndPositionData()
        {
            // Only calculate once
            if (_length.HasValue && _position.HasValue)
                return;

            // Viewing the YZ plane
            double zColumnCenterToHoleColsestToColumn = TeeClip.OffsetFromColumnCenter + TeeClip.ColumnBoundToNearestHole;

            // Triangle --> [top of base plate] to [work line below T-clip hole that's closest to the column bounds]
            AAS(BraceAngle, zColumnCenterToHoleColsestToColumn, out double yTopOfBasePlateToWorkLine, out _);

            // Triangle --> [work line below T-clip hole that's closest to the column bounds] to [T-clip hole that's closest to the column bounds]
            AAS(BraceAngle, WT_FlangeGage / 2, out _, out double yWorkLineToHoleClosestToColumnHole);

            // Y location of T-clip hole that's closest to the column bounds
            double yLowerBounds = BasePlate.THK + yTopOfBasePlateToWorkLine + yWorkLineToHoleClosestToColumnHole;

            // Y location of plenum clip hole that's closest to the bottom of the plenum
            double yUpperBounds = TotalColumnHeight - PlenumDepth - BottomOfPlenumToClipHole;

            // Triangle --> [T-clip hole that's closest to the column bounds] to [plenum clip hole that's closest to the bottom of the plenum]
            double yTriangle = yUpperBounds - yLowerBounds;
            AAS(BraceAngle, out double zTriangle, yTriangle, out double holeToHole);

            // Set backing field
            _length = holeToHole + HoleToEnd * 2;

            //-----

            // Triangle --> [T-clip hole that's closest to the column bounds] to [flange gage and work line intersection point]
            AAS(BraceAngle, out double yHoleClosestToColumn_To_FlangeGageCenterPoint, out double zHoleClosestToColumn_To_FlangeGageCenterPoint, WT_FlangeGage / 2);

            // Position
            double xTranslation = -Width / 2 - Clip_THK/2;
            double yTranslation = BasePlate.THK + yTopOfBasePlateToWorkLine + yWorkLineToHoleClosestToColumnHole + yTriangle/2 - yHoleClosestToColumn_To_FlangeGageCenterPoint;
            double zTranslation = Length / 2 - zColumnCenterToHoleColsestToColumn - zTriangle/2 - zHoleClosestToColumn_To_FlangeGageCenterPoint;

            _position = PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, rX: BraceAngle); 
            
        }
        private void AddOtherSideBrace(ref List<PositionData> list)
        {
            var otherSideBrace = (PositionData)_position;
            otherSideBrace.TranslationX *= -1;
            otherSideBrace.RotationX *= -1;
            otherSideBrace.RotationY = 180;
            list.Add(otherSideBrace);
        }
        private void AddOppositeSideBrace(ref List<PositionData> list)
        {
            var oppositeSideBrace = (PositionData)_position;
            oppositeSideBrace.TranslationZ *= -1;
            oppositeSideBrace.RotationX *= -1;
            list.Add(oppositeSideBrace);
        }
        private void AddOtherOppositeSideBrace(ref List<PositionData> list)
        {
            var otherOppositeSideBrace = (PositionData)_position;
            otherOppositeSideBrace.TranslationX *= -1;
            otherOppositeSideBrace.TranslationZ *= -1;
            otherOppositeSideBrace.RotationY = 180;
            list.Add(otherOppositeSideBrace);
        }


        // Property overrides
        public override bool Enabled => new[] { "T", "TX" }.Contains(BraceType);
        public override string StaticPartNo => "131T";
        public override Shape RawMaterialShape => Shape.Tee;
        public override string SizeOrThickness => WT_Size;
        public override List<PositionData> Position
        {
            get
            {
                var pos = new List<PositionData>
                {
                    (PositionData)_position
                };

                AddOtherSideBrace(ref pos);
                AddOppositeSideBrace(ref pos);
                AddOtherOppositeSideBrace(ref pos);

                return pos;
            }
        }


        // Private properties
        private double? _length;
        private PositionData? _position;
    }
}
