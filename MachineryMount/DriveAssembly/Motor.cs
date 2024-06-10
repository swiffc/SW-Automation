using FileTools.Base;
using MachineryMount.DriveWeldment.Children;
using MachineryMount.DriveWeldment.Children.Derived;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;

namespace MachineryMount.DriveAssembly
{
    internal class Motor : Part
    {
        // Static properties
        public static int Frame
        {
            get { return MotorFrameSize; }
            set { MotorFrameSize = value; }
        }
        public static (double O, double D, double BA, double C, double NW, double E, double F, double U, double AB, double H) Dim => MotorFrame[Frame];
        static public double HP { get; set; }
        static public int RPM { get; set; }
        public static double Shift
        {
            get { return MotorShift; }
            set { MotorShift = value; }
        }



        // Constructor
        public Motor(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Motor", "D", Dim.D);
            EditDimension("Motor", "O", Dim.O);
            EditDimension("Motor", "BA", Dim.BA);
            EditDimension("Motor", "NW", Dim.NW);
            EditDimension("Motor", "C", Dim.C);
            EditDimension("Motor", "E", Dim.E);
            EditDimension("Motor", "F", Dim.F);
            EditDimension("U", "sk:U", Dim.U);
            EditDimension("H", "sk:H", Dim.H);
            EditDimension("Motor", "AB", Dim.AB);
        }


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => "MTR";
        public override Shape RawMaterialShape => 0;
        public override string SizeOrThickness => "";
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tX: -MachineryMount.CenterToCenter, tY: Stringer.Depth + MotorRise, tZ: -MotorShift, rY: -90)
                };
            }
        }
    }
}
