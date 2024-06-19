using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FileTools.CommonData;
using static FileTools.CommonData.CommonData;
using Microsoft.Office.Interop.Excel;
using Bundle.TubeSupports;

namespace Bundle.Misc
{
    internal class Tube : Part
    {
        static public double Camber => Math.Ceiling(TubeSupport.Quantity / 2) * 0.125;


        // Constructor
        public Tube(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Path", Length);
            EditDimension("Camber", "sk:Path", Cambered ? Camber : 0.001);
            EditDimension("Diameter", "sk:Profile", OD);
            EditDimension("Wall", "Tube", WallTHK);
            EditDimension("StripBack", "Front", FrontStripBack);
            EditDimension("StripBack", "Rear", RearStripBack);
            EditDimension("Diameter", "sk:FinsRepresentation", FinOD);
        }


        // Public methods
        public static double GetSlopeAngleDegrees(double slopePerFoot)
        {
            return Math.Atan(slopePerFoot * (Length / 12) / Length) * (180.0 / Math.PI);
        }


        // Private methods
        public static double CamberAtLocation(double x)
        {
            double[] xCurve = { 0, Length / 2, Length };
            double[] yCurve = { 0, Camber, 0 };

            double y = TubeCamberInterpolator.InterpolateAtPoint(xCurve, yCurve, x);

            return y;
        }


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => "Tube";
        public override Shape RawMaterialShape => 0;
        public override string SizeOrThickness => "";
        public override List<PositionData> Position
        {
            get
            {
                var pos = new List<PositionData>();

                double yTranslation = Header61.Y_Location - Header61.Xtop;
                double zTranslation = Length / 2;

                double[] slopesPerFoot = new double[] 
                { 
                    SlopePerFoot.Row1, 
                    SlopePerFoot.Row2, 
                    SlopePerFoot.Row3, 
                    SlopePerFoot.Row4, 
                    SlopePerFoot.Row5, 
                    SlopePerFoot.Row6, 
                    SlopePerFoot.Row7, 
                    SlopePerFoot.Row8, 
                    SlopePerFoot.Row9, 
                    SlopePerFoot.Row10 
                };
                double[] verticalPitches = new double[] 
                { 
                    VerticalPitch._1_2, 
                    VerticalPitch._2_3, 
                    VerticalPitch._3_4, 
                    VerticalPitch._4_5, 
                    VerticalPitch._5_6, 
                    VerticalPitch._6_7, 
                    VerticalPitch._7_8, 
                    VerticalPitch._8_9, 
                    VerticalPitch._9_10, 0 
                };

                for (int i = 0; i < 10; i++)
                {
                    double tubesPerRow = i % 2 == 0 ? Tube_Row_1L : Tube_Row_2L;
                    double xFirstTube = (Math.Ceiling(tubesPerRow) - 1) * HorizPitch / 2;
                    double slopeAngleDegrees = GetSlopeAngleDegrees(slopesPerFoot[i]);

                    pos.Add(PositionData.Create(tX: -xFirstTube, tY: yTranslation, tZ: zTranslation, rX: slopeAngleDegrees));

                    if (verticalPitches[i] == 0)
                        break;

                    yTranslation -= verticalPitches[i];
                }

                return pos;
            }
        }


        // Wrapper properties
        static public double OD
        {
            get { return CommonData.TubeOD; }
            set { CommonData.TubeOD = value; }
        }
        static public double FinOD
        {
            get { return CommonData.FinOD; }
            set { CommonData.FinOD = value; }
        }
        static public double WallTHK
        {
            get { return CommonData.TubeWallTHK; }
            set { CommonData.TubeWallTHK = value; }
        }
        static public double Length
        {
            get { return CommonData.TubeLength; }
            set { CommonData.TubeLength = value; }
        }
        static public double FrontStripBack
        {
            get { return CommonData.FinStripBack_Front; }
            set { CommonData.FinStripBack_Front = value; }
        }
        static public double RearStripBack
        {
            get { return CommonData.FinStripBack_Rear; }
            set { CommonData.FinStripBack_Rear = value; }
        }
        public static int Quantity
        {
            get => TubeQuantity;
            set => TubeQuantity = value;
        }
        public static double HorizPitch
        {
            get => TubeHorizPitch;
            set => TubeHorizPitch = value;
        }


        // Nested classes
        public static class TubeCamberInterpolator
        {
            // Struct to store spline coefficients for each segment
            struct Spline
            {
                public double a, b, c, d, x;
            }

            // Interpolates the Y value at a specific X coordinate using the calculated splines
            public static double InterpolateAtPoint(double[] x, double[] y, double specificX)
            {
                // Calculate spline coefficients for each segment
                Spline[] splines = CalculateSplines(x, y);

                // Calculate and return the corresponding Y value for the specific X
                return Interpolate(splines, specificX);
            }

            // Calculates the spline coefficients for each segment between points
            private static Spline[] CalculateSplines(double[] x, double[] y)
            {
                int n = x.Length; // Number of points
                Spline[] splines = new Spline[n]; // Array to hold the spline coefficients for each segment

                // Initialize splines with X and Y values
                for (int i = 0; i < n; i++)
                {
                    splines[i] = new Spline { x = x[i], a = y[i] };
                }

                // Temporary arrays for the tridiagonal algorithm
                double[] alpha = new double[n - 1];
                double[] beta = new double[n - 1];
                alpha[0] = beta[0] = 0;

                // Tridiagonal algorithm to solve for spline coefficients
                for (int i = 1; i < n - 1; i++)
                {
                    double hi = x[i] - x[i - 1];
                    double hi1 = x[i + 1] - x[i];
                    double A = hi;
                    double C = 2 * (hi + hi1); // The diagonal component
                    double B = hi1;
                    double F = 6 * ((y[i + 1] - y[i]) / hi1 - (y[i] - y[i - 1]) / hi); // Right-hand side

                    double z = A * alpha[i - 1] + C;
                    alpha[i] = -B / z;
                    beta[i] = (F - A * beta[i - 1]) / z;
                }

                // Backward substitution to solve for c coefficients
                splines[n - 1].c = 0;
                for (int i = n - 2; i >= 0; i--)
                {
                    splines[i].c = alpha[i] * splines[i + 1].c + beta[i];
                }

                // Solve for b and d coefficients based on c
                for (int i = n - 1; i > 0; i--)
                {
                    double hi = x[i] - x[i - 1];
                    splines[i].d = (splines[i].c - splines[i - 1].c) / hi;
                    splines[i].b = hi * (2 * splines[i].c + splines[i - 1].c) / 6 + (y[i] - y[i - 1]) / hi;
                }

                return splines;
            }

            // Performs the actual interpolation for a given X value using the spline coefficients
            private static double Interpolate(Spline[] splines, double x)
            {
                // If x is outside the range of x values, return the closest y value
                if (x <= splines[0].x) return splines[0].a;
                int n = splines.Length;
                if (x >= splines[n - 1].x) return splines[n - 1].a;

                // Binary search to find the right segment for interpolation
                int i = 0, j = n - 1;
                while (i + 1 < j)
                {
                    int k = i + (j - i) / 2;
                    if (x <= splines[k].x)
                        j = k;
                    else
                        i = k;
                }

                // Perform the cubic spline interpolation for the found segment
                Spline s = splines[j];
                double dx = x - s.x;
                // The cubic polynomial equation
                return s.a + s.b * dx + s.c * dx * dx / 2 + s.d * dx * dx * dx / 6;
            }
        }
    }
}
