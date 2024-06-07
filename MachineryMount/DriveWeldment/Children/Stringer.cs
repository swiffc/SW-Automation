using FileTools.Base;
using MachineryMount.BeltGuard;
using MachineryMount.DriveAssembly;
using MachineryMount.MotorMount.Children;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using static FileTools.StaticFileTools;
using static Tools.ModelTools;

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
        static public double Length
        {
            get
            {
                double value = Plenum_Width;
                switch (Plenum_Design)
                {
                    case Design.Johnson:
                        value += Beam_Depth;
                        break;
                    case Design.Legacy:
                        value += Beam_Depth - SidePanel_THK * 2 - Beam_FlangeTHK * 2;
                        break;
                    case Design.Standard:
                        value -= SidePanel_THK * 2;
                        break;
                    default:
                        throw new NotImplementedException();
                }
                return value;
            }
        }


        // Constructor
        protected Stringer(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Path", Plenum_Width);

            EditDimension("Hole1", "sk:MountingHoles", MachineryMount.CenterToCenter + Motor.Dim.D - MotorMountPart.SlotInset - MotorMountPart.BeltTensioningAdjustment);
            EditDimension("Hole2", "sk:MountingHoles", MachineryMount.CenterToCenter + Motor.Dim.D - MotorMountPart.Base + MotorMountPart.THK + MotorMountPart.SlotInset + MotorMountPart.BeltTensioningAdjustment);

            EditDimension("Hole1", "sk:BeltGuardHoles", CrossStiffenerC.X_Translation - AssemblyClearance - BeltGuardWld.HoleInset);
            EditDimension("Hole2", "sk:BeltGuardHoles", DriveFrame.Width / 2 - AssemblyClearance - BeltGuardWld.HoleInset);
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
