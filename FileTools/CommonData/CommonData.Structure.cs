using ModelTools;
using static Tools.ModelTools;
using static FileTools.Properties.Settings;
using System;

namespace FileTools.CommonData
{
    public static partial class CommonData
    {
        // Footprint
        static public double Plenum_Width
        {
            get => Default.Plenum_Width;
            set => Default.Plenum_Width = value;
        }
        static public double Plenum_Length => Default.Plenum_Length;
        static public double TotalColumnHeight => Default.TotalColumnHeight;
        static public double FieldColumn_Height
        {
            get
            {
                var value = TotalColumnHeight - Plenum_Depth - MachineryMount_Height + ShippingBeam_Height;
                return value;
            }
        }
        #region MidColumn rules

        static public bool Mid_Columns
        {
            get => Fan_Count != 1 && Default.Mid_Columns;
            set => MidColumnsInternal = value;
        }
        static private bool MidColumnsInternal
        {
            get => Default.Mid_Columns;
            set => Default.Mid_Columns = value;
        }

        #endregion
        static public double BasePlate_THK => Default.BasePlate_THK;

        // Beams
        public static string Beam_Size => Default.Beam_Size;
        static public bool Beams_AreRotated => Default.Beams_AreRotated;
        public static double Beam_Depth
        {
            get
            {
                if (SteelBook.W_Shape.TryGetValue(Beam_Size, out var wShape))
                {
                    Default.Beam_Depth = wShape.Depth;
                }
                else
                {
                    Default.Beam_Depth = 6;
                }
                return Default.Beam_Depth;
            }
            set
            {
                Default.Beam_Depth = value;
            }
        }
        public static double Beam_WebTHK
        {
            get
            {
                if (SteelBook.W_Shape.TryGetValue(Beam_Size, out var wShape))
                {
                    Default.Beam_WebTHK = wShape.WebTHK;
                }
                else
                {
                    Default.Beam_WebTHK = 0.25;
                }
                return Default.Beam_WebTHK;
            }
            set
            {
                Default.Beam_WebTHK = value;
            }
        }
        public static double Beam_FlangeWidth
        {
            get
            {
                if (SteelBook.W_Shape.TryGetValue(Beam_Size, out var wShape))
                {
                    Default.Beam_FlangeWidth = wShape.FlangeWidth;
                }
                else
                {
                    Default.Beam_FlangeWidth = 6;
                }
                return Default.Beam_FlangeWidth;
            }
            set
            {
                Default.Beam_FlangeWidth = value;
            }
        }
        public static double Beam_FlangeTHK
        {
            get
            {
                if (SteelBook.W_Shape.TryGetValue(Beam_Size, out var wShape))
                {
                    Default.Beam_FlangeTHK = wShape.FlgTHK;
                }
                else
                {
                    Default.Beam_FlangeTHK = 0.25;
                }
                return Default.Beam_FlangeTHK;
            }
            set
            {
                Default.Beam_FlangeTHK = value;
            }
        }
        public static double Beam_K
        {
            get
            {
                if (SteelBook.W_Shape.TryGetValue(Beam_Size, out var wShape))
                {
                    Default.Beam_K = wShape.K;
                }
                else
                {
                    Default.Beam_K = 0.625;
                }
                return Default.Beam_K;
            }
            set
            {
                Default.Beam_K = value;
            }
        }
        public static double Beam_K1
        {
            get
            {
                if (SteelBook.W_Shape.TryGetValue(Beam_Size, out var wShape))
                {
                    Default.Beam_K1 = wShape.K1;
                }
                else
                {
                    Default.Beam_K1 = 0.375;
                }
                return Default.Beam_K1;
            }
            set
            {
                Default.Beam_K1 = value;
            }
        }






        // WT Size
        static public double WT_Depth => Default.WT_Depth;
        static public double WT_StemTHK => Default.WT_StemTHK;
        static public double WT_FlangeWidth => Default.WT_FlangeWidth;
        static public double WT_FlangeTHK => Default.WT_FlangeTHK;
        static public double WT_K => Default.WT_K;
        static public double WT_K1 => Default.WT_K1;
        static public double WT_FlangeGage => Default.WT_FlangeGage;


        // Bracing
        static public double ClipHeight => Default.Clip_Height;
        #region BraceType Rules

        public static string BraceType
        {
            get
            {
                return Default.BraceType;
            }
            set
            {
                if (value == "L" || value == "LL" || value == "T" || value == "X" || value == "TX")
                    Default.BraceType = value;
                else
                    Default.BraceType = "L";
            }
        }

        #endregion
        static public double BraceAngle => Default.BraceAngle;
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
        #region HoleDiameter Rules

        public static double HoleDiameter_Structural
        {
            get { return Default.HoleDiameter_Structural; }
            set
            {
                Default.HoleDiameter_Structural = value;
                HoleToEdge = Default.HoleDiameter_Structural > 1 ? 2.0 // Danny said 2" is fine, but logic can be implemented whenever needed
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

            double yPlenumClipHole = TotalColumnHeight - Plenum_Depth - BottomOfPlenumToClipHole;
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
            double zColumnCenterToHoleClosestToColumn = TeeClip_OffsetFromColumnCenter + TeeClip_ColumnBoundToNearestHole;

            // Triangle --> [top of base plate] to [work line below T-clip hole that's closest to the column bounds]
            AAS(BraceAngle, zColumnCenterToHoleClosestToColumn, out double yTopOfBasePlateToWorkLine, out _);

            // Triangle --> [work line below T-clip hole that's closest to the column bounds] to [T-clip hole that's closest to the column bounds]
            AAS(BraceAngle, WT_FlangeGage / 2, out _, out double yWorkLineToHoleClosestToColumnHole);

            // Y location of T-clip hole that's closest to the column bounds
            yLowerBounds_TeeClip = BasePlate_THK + yTopOfBasePlateToWorkLine + yWorkLineToHoleClosestToColumnHole;

            // Y location of plenum clip hole that's closest to the bottom of the plenum
            double yUpperBounds = TotalColumnHeight - Plenum_Depth - BottomOfPlenumToClipHole;

            // Triangle --> [T-clip hole that's closest to the column bounds] to [plenum clip hole that's closest to the bottom of the plenum]
            double yTriangle = yUpperBounds - yLowerBounds_TeeClip;
            AAS(BraceAngle, out double zTriangle, yTriangle, out double holeToHole);

            // Set backing field
            _length = holeToHole + HoleToEnd * 2;

            //-----

            // Triangle --> [T-clip hole that's closest to the column bounds] to [flange gage and work line intersection point]
            AAS(BraceAngle, out double yHoleClosestToColumn_To_FlangeGageCenterPoint, out double zHoleClosestToColumn_To_FlangeGageCenterPoint, WT_FlangeGage / 2);

            // Position
            double xTranslation = -Plenum_Width / 2 - Clip_THK / 2;
            double yTranslation = BasePlate_THK + yTopOfBasePlateToWorkLine + yWorkLineToHoleClosestToColumnHole + yTriangle / 2 - yHoleClosestToColumn_To_FlangeGageCenterPoint;
            double zTranslation = Plenum_Length / 2 - zColumnCenterToHoleClosestToColumn - zTriangle / 2 - zHoleClosestToColumn_To_FlangeGageCenterPoint;

            _position = PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, rX: BraceAngle);

        }
        static double TeeClip_OffsetFromColumnCenter => Beams_AreRotated ? Beam_WebTHK / 2 : Beam_FlangeWidth / 2;
        static double TeeClip_ColumnBoundToNearestHole => Beams_AreRotated ? Beam_FlangeWidth / 2 - Beam_WebTHK / 2 + ColumnBoundsToHole : ColumnBoundsToHole;

    }
}
