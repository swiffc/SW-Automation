using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FileTools.CommonData;
using static FileTools.CommonData.CommonData;

namespace Bundle.Misc
{
    internal class Tube : Part
    {
        // Constructor
        public Tube(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Diameter", "sk:Tube", OD);
            EditDimension("Wall", "sk:Tube", WallTHK);
            EditDimension("Length", "Tube", Length);
            EditDimension("StripBack", "Front", FrontStripBack);
            EditDimension("StripBack", "Rear", RearStripBack);
            EditDimension("Diameter", "sk:Fins", FinOD);
        }


        // Private methods
        double GetSlopeAngleDegrees(double slopePerFoot)
        {
            return Math.Atan(slopePerFoot * (Length / 12) / Length) * (180.0 / Math.PI);
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

                double[] slopesPerFoot = new double[] { SlopePerFoot.Row1, SlopePerFoot.Row2, SlopePerFoot.Row3, SlopePerFoot.Row4, SlopePerFoot.Row5, SlopePerFoot.Row6, SlopePerFoot.Row7, SlopePerFoot.Row8, SlopePerFoot.Row9, SlopePerFoot.Row10 };
                double[] verticalPitches = new double[] { VerticalPitch._1_2, VerticalPitch._2_3, VerticalPitch._3_4, VerticalPitch._4_5, VerticalPitch._5_6, VerticalPitch._6_7, VerticalPitch._7_8, VerticalPitch._8_9, VerticalPitch._9_10, 0 };

                for (int i = 0; i < 10; i++)
                {
                    double tubesPerRow = i % 2 == 0 ? Tube_Row_1L : Tube_Row_2L;
                    double xFirstTube = tubesPerRow / 2 * HorizPitch;
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
    }
}
