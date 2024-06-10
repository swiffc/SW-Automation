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

            Holes_181(out double span, out double count, out double spacing);
            EditDimension("Offset", "sk:Hole", span / 2);
            EditDimension("Count", "sk:Hole", count);
            EditDimension("Spacing", "sk:Hole", spacing);

            Holes_247(out double span2, out double count2, out double spacing2);
            EditDimension("Offset2", "sk:Hole", span2 / 2);
            EditDimension("Count2", "sk:Hole", count2);
            EditDimension("Spacing2", "sk:Hole", spacing2);

            if (TensioningAngle.JackScrew == TensioningAngle.Config.None)
                EditDimension("Location", "sk:JackScrewHoles", Stringer.Depth + 1.375);

        }
        protected override void Features()
        {
            if (TensioningAngle.JackScrew == TensioningAngle.Config.None)
                UnsuppressFeatures("JackScrewHoles");
            else
                SuppressFeatures("JackScrewHoles");
        }


        // Static methods
        public static void Holes_181(out double span, out double count, out double spacing)
        {
            double inset = 3;
            span = DriveFrame.Width - inset * 2;
            HolePattern(span, out count, out spacing, 12);
        }
        public static void Holes_247(out double span, out double count, out double spacing)
        {
            Holes_181(out span, out count, out spacing);
            span -= spacing;
            count -= 1;
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
                    PositionData.Create(tX: Stringer.Length / 2),
                    PositionData.Create(tX: -Stringer.Length / 2, rY: 180)
                };
            }
        }


        // Constants
        const double MountingExtension = 4.5;
        const double Landing = 4.375;
    }
}
