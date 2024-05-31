using FileTools.Base;
using MachineryMount.Mechanicals;
using MachineryMount.MotorMount.Children;
using ModelTools;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;

namespace MachineryMount.DriveWeldment.Children
{
    internal class GuardPlate : Part
    {
        // Static properties
        static public double THK => 0.25;


        // Constructor
        public GuardPlate(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("DriveWidth", "sk:Plate", DriveFrame.Width);
            EditDimension("ToMotorMount", "sk:Plate", MachineryMount.CenterToCenter + Motor.Dim.D - MotorMountPart.Base + MotorMountPart.BeltTensioningAdjustment);
        }


        // Property overrides
        public override bool Enabled => ForcedDraft;
        public override string StaticPartNo => "246";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tY: Stringer.Depth)
                };
            }
        }
    }
}
