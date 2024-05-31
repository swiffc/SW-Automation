using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using static FileTools.StaticFileTools;

namespace MachineryMount.DriveWeldment.Children
{
    internal abstract class Stringer : Part
    {
        // Static properties
        static public int Priority => 1;
        public static string Size
        {
            get { return Stringer_Size; }
            set { Stringer_Size = value; }
        }
        public static double Depth => Stringer_Depth;
        public static double FlangeWidth
        {
            get
            {
                if (!_flangeWidth.HasValue)
                {
                    throw new InvalidOperationException("FlangeWidth has not been set.");
                }
                return _flangeWidth.Value;
            }
            internal set
            {
                _flangeWidth = value;
            }
        }
        public static double WebTHK
        {
            get
            {
                if (!_webTHK.HasValue)
                {
                    throw new InvalidOperationException("WebTHK has not been set.");
                }
                return _webTHK.Value;
            }
            internal set
            {
                _webTHK = value;
            }
        }
        static public double Length => Plenum_Width; // temp


        // Constructor
        protected Stringer(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Path", Plenum_Width);
        }
        protected override void Features()
        {
            EditFeature_StructuralMemberSize(Size, out double flangeWidth, out double webTHK);
            FlangeWidth = flangeWidth;
            WebTHK = webTHK;
        }


        // Property overrides
        public override bool Enabled => true;
        public override Shape RawMaterialShape => Shape.Channel;
        public override string SizeOrThickness => Size;


        // Backing fields
        internal static double? _flangeWidth;
        internal static double? _webTHK;
    }
}
