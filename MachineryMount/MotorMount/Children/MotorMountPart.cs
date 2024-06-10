using FileTools.Base;
using MachineryMount.DriveWeldment;
using MachineryMount.DriveWeldment.Children;
using MachineryMount.DriveAssembly;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;

namespace MachineryMount.MotorMount.Children
{
    internal class MotorMountPart : Part
    {
        // Static properties
        static public double Base => Motor.Dim.O + 4.625;
        static public double Back => Motor.Dim.C - Motor.Dim.BA - Motor.Dim.NW + MotorRise;
        static public double THK => 0.25;
        static public double BeltTensioningAdjustment => 1.5;
        static public double DriveCenterToBackingExterior => MachineryMount.CenterToCenter + Motor.Dim.D + THK;
        static public double SlotInset => 1.5;


        // Constructor
        public MotorMountPart(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Base", "sk:Plate", Base);
            EditDimension("Back", "sk:Plate", Back);
            EditDimension("D", "sk:Plate", Motor.Dim.D);
            EditDimension("Width", "Plate", DriveFrame.Width + Stringer.FlangeWidth * 2);

            EditDimension("D", "sk:MotorCope", Motor.Dim.D);
            EditDimension("O", "sk:MotorCope", Motor.Dim.O);

            EditDimension("H", "sk:MotorHoles", Motor.Dim.H);
            EditDimension("F", "sk:MotorHoles", Motor.Dim.F);
            EditDimension("E", "sk:MotorHoles", Motor.Dim.E);
            EditDimension("Rise", "sk:MotorHoles", MotorRise);

            EditDimension("D1", "MotorCenter", Motor.Shift);
        }


        // Property overrides
        public override bool Enabled => ForcedDraft;
        public override string StaticPartNo => "234P";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create()
                };
            }
        }
    }
}
