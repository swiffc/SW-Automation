using FileTools.Base;
using ModelTools;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using static FileTools.StaticFileTools;


namespace Structure.Columns.Derived.Children
{
    internal class Beam : Part
    {
        // Static properties
        static internal string Size { get; set; } = "6x15";
        internal static double Depth
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
                    return 6;
                }
            }
            set
            {
                _depth = value;
            }
        }
        internal static double WebTHK
        {
            get
            {
                if (_webTHK.HasValue)
                {
                    return _webTHK.Value;
                }
                if (SteelBook.W_Shape.TryGetValue(Size, out var wShape))
                {
                    return wShape.WebTHK;
                }
                return 0.25;
            }
            set { _webTHK = value; }
        }
        internal static double FlangeWidth
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
                return 6;
            }
            set { _flangeWidth = value; }
        }
        internal static double FlangeTHK
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
                return 0.25;
            }
            set { _flangeTHK = value; }
        }
        internal static double K
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
                return 0.625;
            }
            set { _k = value; }
        }
        internal static double K1
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
                return 0.375;
            }
            set { _k1 = value; }
        }
        internal static double FlangeGage
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
                return 3.5;
            }
            set { _flangeGage = value; }
        }
        internal static double WebGage
        {
            get
            {
                if (_webGage.HasValue)
                {
                    return _webGage.Value;
                }
                if (SteelBook.W_Shape.TryGetValue(Size, out var wShape))
                {
                    return wShape.WebGage;
                }
                return 2.25;
            }
            set { _webGage = value; }
        }
        static internal bool IsRotated { get; set; } = false;
        static internal double LocalLength => FieldColumn.Height - BasePlate.THK - EndPlate.THK;


        // Constructor
        public Beam(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "Beam", LocalLength);

            EditDimension("Depth", "sk:Beam", Depth);
            EditDimension("WebTHK", "sk:Beam", WebTHK);
            EditDimension("FlangeWidth", "sk:Beam", FlangeWidth);
            EditDimension("FlangeTHK", "sk:Beam", FlangeTHK);
            EditDimension("K", "sk:Beam", K);
            EditDimension("K1", "sk:Beam", K1);
        }


        // Public methods
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


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => "101P";
        public override Shape RawMaterialShape => Shape.Beam;
        public override string SizeOrThickness => Size;
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tY: BasePlate.THK, rY:IsRotated ? 90 : 0)
                };
            }
        }


        // Private properties
        private static double? _depth;
        private static double? _webTHK;
        private static double? _flangeWidth;
        private static double? _flangeTHK;
        private static double? _k;
        private static double? _k1;
        private static double? _flangeGage;
        private static double? _webGage;
    }
}
