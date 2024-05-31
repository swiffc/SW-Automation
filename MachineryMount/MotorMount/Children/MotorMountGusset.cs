using FileTools.Base;
using MachineryMount.DriveWeldment;
using MachineryMount.Mechanicals;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Tools.ModelTools;

namespace MachineryMount.MotorMount.Children
{
    internal class MotorMountGusset : Part
    {
        // Constructor
        public MotorMountGusset(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Base", "sk:Plate", MotorMountPart.Base - MotorMountPart.THK - WeldClearance * 2);
            EditDimension("Back", "sk:Plate", MotorMountPart.Back - MotorMountPart.THK);
        }


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => "233";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => MotorMountPart.THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                double xTranslation = -Motor.Dim.D + WeldClearance;
                double yTranslation = MotorMountPart.THK;
                double zTranslation = (DriveFrame.Width + MotorMountPart.THK) / 2;

                return new List<PositionData>
                {
                    PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation),
                    PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: -zTranslation),
                };
            }
        }
    }
}
