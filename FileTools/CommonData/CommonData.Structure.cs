using ModelTools;
using static Tools.ModelTools;
using static FileTools.Properties.Settings;
using System;

namespace FileTools.CommonData
{
    public static partial class CommonData
    {
        // Footprint
        static public double Width { get; set; } = 108;
        static public double Length { get; set; } = 240;
        static public double TotalColumnHeight { get; set; } = 120;
        static private bool _midColumns = true;
        #region MidColumn rules

        static public bool MidColumns
        {
            get => FanCount != 1 && _midColumns;
            set => MidColumnsInternal = value;
        }
        static private bool MidColumnsInternal
        {
            get => _midColumns;
            set => _midColumns = value;
        }
        static public double BasePlate_THK { get; set; } = 0.5;

        #endregion


        // Beams
        public static string Beam_Size => Default.Beam_Size;
        static public bool Beams_AreRotated => Default.Beams_AreRotated;
        public static double Beam_Depth
        {
            get
            {
                if (_depth.HasValue)
                {
                    return _depth.Value;
                }

                if (SteelBook.W_Shape.TryGetValue(Beam_Size, out var wShape))
                {
                    return wShape.Depth;
                }
                else
                {
                    return 6;
                }
            }
            set
            {
                _depth = value;
            }
        }
        public static double Beam_WebTHK
        {
            get
            {
                if (_webTHK.HasValue)
                {
                    return _webTHK.Value;
                }
                if (SteelBook.W_Shape.TryGetValue(Beam_Size, out var wShape))
                {
                    return wShape.WebTHK;
                }
                return 0.25;
            }
            set { _webTHK = value; }
        }
        public static double Beam_FlangeWidth
        {
            get
            {
                if (_flangeWidth.HasValue)
                {
                    return _flangeWidth.Value;
                }
                if (SteelBook.W_Shape.TryGetValue(Beam_Size, out var wShape))
                {
                    return wShape.FlangeWidth;
                }
                return 6;
            }
            set { _flangeWidth = value; }
        }
        public static double Beam_FlangeTHK
        {
            get
            {
                if (_flangeTHK.HasValue)
                {
                    return _flangeTHK.Value;
                }
                if (SteelBook.W_Shape.TryGetValue(Beam_Size, out var wShape))
                {
                    return wShape.FlgTHK;
                }
                return 0.25;
            }
            set { _flangeTHK = value; }
        }
        public static double Beam_K
        {
            get
            {
                if (_k.HasValue)
                {
                    return _k.Value;
                }
                if (SteelBook.W_Shape.TryGetValue(Beam_Size, out var wShape))
                {
                    return wShape.K;
                }
                return 0.625;
            }
            set { _k = value; }
        }
        public static double Beam_K1
        {
            get
            {
                if (_k1.HasValue)
                {
                    return _k1.Value;
                }
                if (SteelBook.W_Shape.TryGetValue(Beam_Size, out var wShape))
                {
                    return wShape.K1;
                }
                return 0.375;
            }
            set { _k1 = value; }
        }
        public static double Beam_FlangeGage
        {
            get
            {
                if (_flangeGage.HasValue)
                {
                    return _flangeGage.Value;
                }
                if (SteelBook.W_Shape.TryGetValue(Beam_Size, out var wShape))
                {
                    return wShape.FlangeGage;
                }
                return 3.5;
            }
            set { _flangeGage = value; }
        }
        public static double Beam_WebGage
        {
            get
            {
                if (_webGage.HasValue)
                {
                    return _webGage.Value;
                }
                if (SteelBook.W_Shape.TryGetValue(Beam_Size, out var wShape))
                {
                    return wShape.WebGage;
                }
                return 2.25;
            }
            set { _webGage = value; }
        }
        public static void ResetSize()
        {
            _depth = null;
            _webTHK = null;
            _flangeWidth = null;
            _flangeTHK = null;
            _k = null;
            _k1 = null;
            _flangeGage = null;
            _webGage = null;
        }
        private static double? _depth;
        private static double? _webTHK;
        private static double? _flangeWidth;
        private static double? _flangeTHK;
        private static double? _k;
        private static double? _k1;
        private static double? _flangeGage;
        private static double? _webGage;


        // WT Size
        static public string WT_Size { get; set; } = "WT2x6.5";
        static public double WT_Depth { get; set; } = 2.125;
        static public double WT_StemTHK { get; set; } = 0.25;
        static public double WT_FlangeWidth { get; set; } = 4;
        static public double WT_FlangeTHK { get; set; } = 0.375;
        static public double WT_K { get; set; } = 0.6875;
        static public double WT_K1 { get; set; } = 0.4375;
        static public double WT_FlangeGage { get; set; } = 2.25;


        // Bracing
        static public double ClipHeight { get; set; } = 20;
        private static string _braceType = "TX";
        #region BraceType Rules

        public static string BraceType
        {
            get
            {
                return _braceType;
            }
            set
            {
                if (value == "L" || value == "LL" || value == "T" || value == "X" || value == "TX")
                    _braceType = value;
                else
                    _braceType = "L";
            }
        }

        #endregion
        static public double BraceAngle { get; set; } = 30;
        static public double HoleToEnd { get; set; } = 1.125;
        public static double ColumnBoundsToHole => 2.0;
        public static double PlenumBoundsToHole => 2.5;
        static public double Clip_THK => Default.Clip_THK;
        private static double _holeToEdge = 2.0;
        #region HoleToEdge Rules

        public static double HoleToEdge
        {
            get { return _holeToEdge; }
            private set { _holeToEdge = value; } 
        }

        #endregion
        private static double _holeDiameter = 0.8125;
        #region HoleDiameter Rules

        public static double HoleDiameter_Structural
        {
            get { return _holeDiameter; }
            set
            {
                _holeDiameter = value;
                HoleToEdge = _holeDiameter > 1 ? 2.0 // Danny said 2" is fine, but logic can be implemented whenever needed
                    : 2.0;
            }
        }

        #endregion


        // Public methods
        public static double KneeBraceLength()
        {
            KneeBraceTrig(out double kneeBraceLength, out _, out _);
            return kneeBraceLength;
        }
        public static double ColumnCenterToPlenumEndClipHole()
        {
            KneeBraceTrig(out _, out double xTranslation, out _);
            return xTranslation;
        }
        public static double ColumnCenterToPlenumSideClipHole()
        {
            KneeBraceTrig(out _, out _, out double zTranslation);
            return zTranslation;
        }


        // Private methods
        private static void KneeBraceTrig(out double kneeBraceLength, out double xTranslation, out double zTranslation)
        {
            CalculateLengthAndPositionData(out _, out double yTranslation_TeeClip, out _);

            double yPlenumClipHole = TotalColumnHeight - PlenumDepth - BottomOfPlenumToClipHole;
            double yStructureClipHole = BraceType.Contains("L") ? ClipHeight : yTranslation_TeeClip;

            // Triangle --> [structure knee brace clip hole] to [plenum knee brace clip hole]
            double horz;
            double vert = yPlenumClipHole - yStructureClipHole;
            double diag;
            double angle = BraceAngle;

            AAS(angle, out horz, vert, out diag);

            kneeBraceLength = diag + HoleToEnd * 2;
            zTranslation = (Beams_AreRotated ? Beam_FlangeWidth : Beam_Depth) / 2 + ColumnBoundsToHole + horz;
            xTranslation = (Beams_AreRotated ? Beam_Depth : Beam_FlangeWidth) / 2 + ColumnBoundsToHole + horz;
        }
        public static void CalculateLengthAndPositionData(out double _length, out double yLowerBounds_TeeClip, out PositionData _position)
        {
            // Viewing the YZ plane
            double zColumnCenterToHoleColsestToColumn = TeeClip_OffsetFromColumnCenter + TeeClip_ColumnBoundToNearestHole;

            // Triangle --> [top of base plate] to [work line below T-clip hole that's closest to the column bounds]
            AAS(BraceAngle, zColumnCenterToHoleColsestToColumn, out double yTopOfBasePlateToWorkLine, out _);

            // Triangle --> [work line below T-clip hole that's closest to the column bounds] to [T-clip hole that's closest to the column bounds]
            AAS(BraceAngle, WT_FlangeGage / 2, out _, out double yWorkLineToHoleClosestToColumnHole);

            // Y location of T-clip hole that's closest to the column bounds
            yLowerBounds_TeeClip = BasePlate_THK + yTopOfBasePlateToWorkLine + yWorkLineToHoleClosestToColumnHole;

            // Y location of plenum clip hole that's closest to the bottom of the plenum
            double yUpperBounds = TotalColumnHeight - PlenumDepth - BottomOfPlenumToClipHole;

            // Triangle --> [T-clip hole that's closest to the column bounds] to [plenum clip hole that's closest to the bottom of the plenum]
            double yTriangle = yUpperBounds - yLowerBounds_TeeClip;
            AAS(BraceAngle, out double zTriangle, yTriangle, out double holeToHole);

            // Set backing field
            _length = holeToHole + HoleToEnd * 2;

            //-----

            // Triangle --> [T-clip hole that's closest to the column bounds] to [flange gage and work line intersection point]
            AAS(BraceAngle, out double yHoleClosestToColumn_To_FlangeGageCenterPoint, out double zHoleClosestToColumn_To_FlangeGageCenterPoint, WT_FlangeGage / 2);

            // Position
            double xTranslation = -Width / 2 - Clip_THK / 2;
            double yTranslation = BasePlate_THK + yTopOfBasePlateToWorkLine + yWorkLineToHoleClosestToColumnHole + yTriangle / 2 - yHoleClosestToColumn_To_FlangeGageCenterPoint;
            double zTranslation = Length / 2 - zColumnCenterToHoleColsestToColumn - zTriangle / 2 - zHoleClosestToColumn_To_FlangeGageCenterPoint;

            _position = PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, rX: BraceAngle);

        }
        static double TeeClip_OffsetFromColumnCenter => Beams_AreRotated ? Beam_WebTHK / 2 : Beam_FlangeWidth / 2;
        static double TeeClip_ColumnBoundToNearestHole => Beams_AreRotated ? Beam_FlangeWidth / 2 - Beam_WebTHK / 2 + ColumnBoundsToHole : ColumnBoundsToHole;

    }
}
