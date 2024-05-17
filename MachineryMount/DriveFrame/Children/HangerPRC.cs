using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;

namespace MachineryMount.DriveFrame.Children
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


        // Constructor
        public HangerPRC(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Width", "sk:PRC", DriveFrame.Width);
            EditDimension("Depth", "sk:PRC", FlangeDepth);
            EditDimension("Height", "PRC", DriveFrame.Height + MountingExtension);
            EditDimension("Angle", "sk:Flange", Angle);
            EditDimension("StringerDepth", "sk:Flange", Stringer.Depth);
        }


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => "241";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => "0.25";
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tZ: Plenum_Width / 2, rY: 180),
                    PositionData.Create(tZ: -Plenum_Width / 2)
                };
            }
        }


        // Constants
        const double MountingExtension = 4.5;
        const double Landing = 4.375;
    }
}
