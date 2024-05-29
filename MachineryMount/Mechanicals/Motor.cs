using FileTools.Base;
using MachineryMount.DriveWeldment.Children;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;

namespace MachineryMount.Mechanicals
{
    internal class Motor : Part
    {
        // Static properties
        public static int Frame
        {
            get { return MotorFrameSize; }
            set { MotorFrameSize = value; }
        }
        public static double Y_Location => 1.5;
        public static (double O, double D, double BA, double C, double NW, double E, double F, double U, double AB) Dim => MotorDimensions[Frame];




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
                    PositionData.Create(tX: -MachineryMount.CenterToCenter, tY: Stringer.Depth + Y_Location, rY: -90)
                };
            }
        }


        // Dictionaries
        public static Dictionary<double, (double O, double D, double BA, double C, double NW, double E, double F, double U, double AB)> MotorDimensions =
            new Dictionary<double, (double O, double D, double BA, double C, double NW, double E, double F, double U, double AB)>
            {
            //    Size       O,          D,          BA,         C,         NW,         E,           F,         U,           AB
                { 182,      (8.8750,     4.5000,     2.7500,     14.8125,   2.7500,     3.7500,      4.5000,    4.5000,      7.3750) },
                { 184,      (8.8750,     4.5000,     2.7500,     16.2500,   2.7500,     3.7500,      5.5000,    1.1250,      7.3750) },
                { 213,      (10.6250,    5.2500,     5.2500,     20.3125,   3.3750,     4.2500,      5.5000,    1.3750,      9.0625) },
                { 215,      (10.6250,    5.2500,     5.2500,     20.3125,   3.3750,     4.2500,      7.0000,    1.3750,      9.0625) },
                { 254,      (12.6250,    6.2500,     4.2500,     25.8125,   4.0000,     5.0000,      8.2500,    1.6250,      9.9375) },
                { 256,      (12.6250,    6.2500,     4.2500,     25.8125,   4.0000,     5.0000,     10.0000,    1.6250,      9.9375) },
                { 284,      (14.1875,    7.0000,     4.7500,     29.4375,   4.6250,     5.5000,      9.5000,    1.8750,     13.4375) },
                { 286,      (14.1817,    7.0000,     4.7500,     29.4375,   4.6250,     5.5000,     11.0000,    1.8750,     13.4375) },
                { 324,      (15.9375,    8.0000,     5.2500,     32.1250,   5.2500,     6.2500,     10.5000,    2.1250,     15.7500) },
                { 326,      (15.9375,    8.0000,     5.2500,     32.1250,   5.2500,     6.2500,     12.0000,    2.1250,     15.7500) },
                { 364,      (17.8125,    9.0000,     5.8750,     35.6250,   5.8750,     7.0000,     11.2500,    2.3750,     17.6875) },
                { 365,      (17.8125,    9.0000,     5.8750,     35.6250,   5.8750,     7.0000,     12.2500,    2.3750,     17.6875) },
                { 404,      (19.9375,   10.0000,     6.2500,     39.5000,   7.2500,     8.0000,     12.2500,    2.8750,     17.5000) },
                { 405,      (19.9375,   10.0000,     6.2500,     39.5000,   7.2500,     8.0000,     13.7500,    2.8750,     17.5000) },
            };
    }
}
