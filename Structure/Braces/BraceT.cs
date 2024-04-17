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
using static FileTools.SharedProperties;

namespace Structure.Braces
{
    internal class BraceT : Part
    {
        // Static properties
        static public string Size { get; set; } = "WT2x6.5";
        static public double HoleToEnd => 1.125;
        static public double Depth
        {
            get
            {
                if (_depth.HasValue)
                {
                    return _depth.Value;
                }

                if (SteelBook.W_Shape.TryGetValue(Size, out var wShape))
                {
                    return wShape.Depth;
                }
                else
                {
                    return 2.125;
                }
            }
            set
            {
                _depth = value;
            }
        }
        static public double StemTHK
        {
            get
            {
                if (_stemTHK.HasValue)
                {
                    return _stemTHK.Value;
                }
                if (SteelBook.W_Shape.TryGetValue(Size, out var wShape))
                {
                    return wShape.WebTHK;
                }
                return 0.25;
            }
            set { _stemTHK = value; }
        }
        static public double FlangeWidth
        {
            get
            {
                if (_flangeWidth.HasValue)
                {
                    return _flangeWidth.Value;
                }
                if (SteelBook.W_Shape.TryGetValue(Size, out var wShape))
                {
                    return wShape.FlangeWidth;
                }
                return 4;
            }
            set { _flangeWidth = value; }
        }
        static public double FlangeTHK
        {
            get
            {
                if (_flangeTHK.HasValue)
                {
                    return _flangeTHK.Value;
                }
                if (SteelBook.W_Shape.TryGetValue(Size, out var wShape))
                {
                    return wShape.FlgTHK;
                }
                return .375;
            }
            set { _flangeTHK = value; }
        }
        static public double K
        {
            get
            {
                if (_k.HasValue)
                {
                    return _k.Value;
                }
                if (SteelBook.W_Shape.TryGetValue(Size, out var wShape))
                {
                    return wShape.K;
                }
                return 0.6875;
            }
            set { _k = value; }
        }
        static public double K1
        {
            get
            {
                if (_k1.HasValue)
                {
                    return _k1.Value;
                }
                if (SteelBook.W_Shape.TryGetValue(Size, out var wShape))
                {
                    return wShape.K1;
                }
                return 0.4375;
            }
            set { _k1 = value; }
        }
        static public double FlangeGage
        {
            get
            {
                if (_flangeGage.HasValue)
                {
                    return _flangeGage.Value;
                }
                if (SteelBook.W_Shape.TryGetValue(Size, out var wShape))
                {
                    return wShape.FlangeGage;
                }
                return 2.25;
            }
            set { _flangeGage = value; }
        }


        // Constructor
        public BraceT(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            CalculateLengthAndPositionData();
            EditDimension("Length", "T", (double)_length);
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
            AAS(BraceAngle, FlangeGage / 2, out _, out double yWorkLineToHoleClosestToColumnHole);

            // Y location of T-clip hole that's closest to the column bounds
            double yLowerBounds = BasePlate.THK + yTopOfBasePlateToWorkLine + yWorkLineToHoleClosestToColumnHole;

            // Y location of plenum clip hole that's closest to the bottom of the plenum
            double yUpperBounds = ColumnHeight - PlenumDepth - PlenumClipHole;

            // Triangle --> [T-clip hole that's closest to the column bounds] to [plenum clip hole that's closest to the bottom of the plenum]
            double yTriangle = yUpperBounds - yLowerBounds;
            AAS(BraceAngle, out double zTriangle, yTriangle, out double holeToHole);

            // Set backing field
            _length = holeToHole + HoleToEnd * 2;


            // Triangle --> [T-clip hole that's closest to the column bounds] to [flange gage and work line intersection point]
            AAS(BraceAngle, out double yHoleClosestToColumn_To_FlangeGageCenterPoint, out double zHoleClosestToColumn_To_FlangeGageCenterPoint, FlangeGage / 2);

            // Position
            double xTranslation = -Width / 2 - Clip.THK/2;
            double yTranslation = BasePlate.THK + yTopOfBasePlateToWorkLine + yWorkLineToHoleClosestToColumnHole + yTriangle/2 - yHoleClosestToColumn_To_FlangeGageCenterPoint;
            double zTranslation = Length / 2 - zColumnCenterToHoleColsestToColumn - zTriangle/2 - zHoleClosestToColumn_To_FlangeGageCenterPoint;

            _position = PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, rX: BraceAngle); 
            
        }


        // Property overrides
        public override bool Enabled => new[] { "T", "TX" }.Contains(BraceType);
        public override string StaticPartNo => "131T";
        public override Shape RawMaterialShape => Shape.Tee;
        public override string SizeOrThickness => Size;
        public override List<PositionData> Position
        {
            get
            {
                var pos = new List<PositionData>
                {
                    (PositionData)_position
                };

                var otherSideBrace = (PositionData)_position;
                otherSideBrace.TranslationX *= -1;
                otherSideBrace.RotationX *= -1;
                otherSideBrace.RotationY = 180;
                pos.Add(otherSideBrace);

                var oppositeSideBrace = (PositionData)_position;
                oppositeSideBrace.TranslationZ *= -1;
                oppositeSideBrace.RotationX *= -1;
                pos.Add(oppositeSideBrace);

                var otherOppositeSideBrace = (PositionData)_position;
                otherOppositeSideBrace.TranslationX *= -1;
                otherOppositeSideBrace.TranslationZ *= -1;
                otherOppositeSideBrace.RotationY = 180;
                pos.Add(otherOppositeSideBrace);

                return pos;
            }
        }


        // Private properties
        private static double? _depth;
        private static double? _stemTHK;
        private static double? _flangeWidth;
        private static double? _flangeTHK;
        private static double? _k;
        private static double? _k1;
        private static double? _flangeGage;
        private double? _length;
        private PositionData? _position;
    }
}
