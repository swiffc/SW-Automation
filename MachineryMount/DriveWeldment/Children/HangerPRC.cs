using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using static Tools.ModelTools;

namespace MachineryMount.DriveWeldment.Children
{
    internal class HangerPRC : Part
    {
        // Static properties
        static public double FlangeDepth
        {
            get
            {
                double angleInRadians = Math.PI * Angle / 180.0;
                return (((DriveFrame.Height - Stringer.Depth) - Stringer.Depth - MountingExtension) * Math.Tan(angleInRadians)) + Landing;
            }
        }
        static private double Angle => DriveFrame.Width > 36 ? 30 : 20;
        static public double THK => 0.25;
        static public double Width => DriveFrame.Width - WeldClearance * 2;


        // Constructor
        public HangerPRC(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Width", "sk:PRC", Width);
            EditDimension("Depth", "sk:PRC", FlangeDepth);
            EditDimension("Height", "PRC", DriveFrame.Height + MountingExtension);
            EditDimension("Angle", "sk:Flange", Angle);
            EditDimension("StringerDepth", "sk:Flange", Stringer.Depth + 
                (TensioningAngle.JackScrew == TensioningAngle.Config.ShiftedInside ? TensioningAngle.Leg : 0));
        }


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => "241";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tX: Plenum_Width / 2),
                    PositionData.Create(tX: -Plenum_Width / 2, rY: 180)
                };
            }
        }


        // Constants
        const double MountingExtension = 4.5;
        const double Landing = 4.375;
    }
}
