using ModelTools;
using System;
using System.Windows.Forms;
using static FileTools.Properties.Settings;

namespace FileTools.CommonData
{
    public static partial class CommonData
    {
        // Bundle
        static public bool HasHeaderPads { get; set; } = true;
        static public bool IsToedOut { get; set; } = false;


        // Wired to UI
        public static double Bundle_Width
        {
            get { return Default.Bundle_Width; }
            set { Default.Bundle_Width = value; }
        }
        public static double SideFrame_THK
        {
            get
            {
                return Default.SideFrame_THK;
            }
            set
            {
                if (BendTable.R.ContainsKey(value))
                {
                    Default.SideFrame_THK = value;
                }
                else
                {
                    MessageBox.Show("Invalid sideframe thickness", "Error",
                        MessageBoxButtons.OK,
                        MessageBoxIcon.Error);
                    Default.SideFrame_THK = Default.SideFrame_THK;
                }
            }
        }
        public static double SideFrame_Depth
        {
            get { return Default.SideFrame_Depth; }
            set { Default.SideFrame_Depth = value; }
        }


        public static double TubeLength
        {
            get => Default.TubeLength;
            set => Default.TubeLength = value;
        }
        public static double TubeProjection
        {
            get => Default.TubeStickThru;
            set => Default.TubeStickThru = value;
        }
        public static bool HeadersOutsideFrames
        {
            get => Default.HeadersOutsideFrames;
            set => Default.HeadersOutsideFrames = value;
        }
        public static double FinStripBack_Front
        {
            get => Default.FinStripBack_Front;
            set => Default.FinStripBack_Front = value;
        }
        public static double FinStripBack_Rear
        {
            get => Default.FinStripBack_Rear;
            set => Default.FinStripBack_Rear = value;
        }
        public static double TubeOD
        {
            get => Default.TubeOD;
            set => Default.TubeOD = value;
        }
        public static double TubeWallTHK
        {
            get => Default.TubeWallTHK;
            set => Default.TubeWallTHK = value;
        }
        public static double FinOD
        {
            get => Default.FinOD;
            set => Default.FinOD = value;
        }
        public static double FinTHK
        {
            get => Default.FinTHK;
            set => Default.FinTHK = value;
        }
        public static int TubeQuantity
        {
            get => Default.TubeQuantity;
            set => Default.TubeQuantity = value;
        }
        public static int Tube_Row_1L
        {
            get => Default.Tubes_Row_1L;
            set => Default.Tubes_Row_1L = value;
        }
        public static int Tube_Row_2L
        {
            get => Default.Tubes_Row_2L;
            set => Default.Tubes_Row_2L = value;
        }
        public static double TubeHorizPitch
        {
            get => Default.TubeHorizPitch;
            set => Default.TubeHorizPitch = value;
        }
        static public SlopeAngles SlopePerFoot => new SlopeAngles();
        public class SlopeAngles
        {
            public double Row1
            {
                get
                {
                    double frontPitch =
                        Header61.Y_Location -
                        Header61.Xtop;

                    double rearPitch =
                        Header62.Y_Location -
                        Header62.Xtop;

                    return (frontPitch - rearPitch) / (TubeLength / 12);
                }
            }
            public double Row2
            {
                get
                {
                    double frontPitch =
                        Header61.Y_Location -
                        Header61.Xtop -
                        FrontVerticalPitch._1_2;

                    double rearPitch =
                        Header62.Y_Location -
                        Header62.Xtop -
                        RearVerticalPitch._1_2;

                    return (frontPitch - rearPitch) / (TubeLength / 12);
                }
            }
            public double Row3
            {
                get
                {
                    double frontPitch =
                        Header61.Y_Location -
                        Header61.Xtop -
                        FrontVerticalPitch._1_2 -
                        FrontVerticalPitch._2_3;

                    double rearPitch =
                        Header62.Y_Location -
                        Header62.Xtop -
                        RearVerticalPitch._1_2 -
                        RearVerticalPitch._2_3;

                    return (frontPitch - rearPitch) / (TubeLength / 12);
                }
            }
            public double Row4
            {
                get
                {
                    double frontPitch =
                        Header61.Y_Location -
                        Header61.Xtop -
                        FrontVerticalPitch._1_2 -
                        FrontVerticalPitch._2_3 -
                        FrontVerticalPitch._3_4;

                    double rearPitch =
                        Header62.Y_Location -
                        Header62.Xtop -
                        RearVerticalPitch._1_2 -
                        RearVerticalPitch._2_3 -
                        RearVerticalPitch._3_4;

                    return (frontPitch - rearPitch) / (TubeLength / 12);
                }
            }
            public double Row5
            {
                get
                {
                    double frontPitch =
                        Header61.Y_Location -
                        Header61.Xtop -
                        FrontVerticalPitch._1_2 -
                        FrontVerticalPitch._2_3 -
                        FrontVerticalPitch._3_4 -
                        FrontVerticalPitch._4_5;

                    double rearPitch =
                        Header62.Y_Location -
                        Header62.Xtop -
                        RearVerticalPitch._1_2 -
                        RearVerticalPitch._2_3 -
                        RearVerticalPitch._3_4 -
                        RearVerticalPitch._4_5;

                    return (frontPitch - rearPitch) / (TubeLength / 12);
                }
            }
            public double Row6
            {
                get
                {
                    double frontPitch =
                        Header61.Y_Location -
                        Header61.Xtop -
                        FrontVerticalPitch._1_2 -
                        FrontVerticalPitch._2_3 -
                        FrontVerticalPitch._3_4 -
                        FrontVerticalPitch._4_5 -
                        FrontVerticalPitch._5_6;

                    double rearPitch =
                        Header62.Y_Location -
                        Header62.Xtop -
                        RearVerticalPitch._1_2 -
                        RearVerticalPitch._2_3 -
                        RearVerticalPitch._3_4 -
                        RearVerticalPitch._4_5 -
                        RearVerticalPitch._5_6;

                    return (frontPitch - rearPitch) / (TubeLength / 12);
                }
            }
            public double Row7
            {
                get
                {
                    double frontPitch =
                        Header61.Y_Location -
                        Header61.Xtop -
                        FrontVerticalPitch._1_2 -
                        FrontVerticalPitch._2_3 -
                        FrontVerticalPitch._3_4 -
                        FrontVerticalPitch._4_5 -
                        FrontVerticalPitch._5_6 -
                        FrontVerticalPitch._6_7;

                    double rearPitch =
                        Header62.Y_Location -
                        Header62.Xtop -
                        RearVerticalPitch._1_2 -
                        RearVerticalPitch._2_3 -
                        RearVerticalPitch._3_4 -
                        RearVerticalPitch._4_5 -
                        RearVerticalPitch._5_6 -
                        RearVerticalPitch._6_7;

                    return (frontPitch - rearPitch) / (TubeLength / 12);
                }
            }
            public double Row8
            {
                get
                {
                    double frontPitch =
                        Header61.Y_Location -
                        Header61.Xtop -
                        FrontVerticalPitch._1_2 -
                        FrontVerticalPitch._2_3 -
                        FrontVerticalPitch._3_4 -
                        FrontVerticalPitch._4_5 -
                        FrontVerticalPitch._5_6 -
                        FrontVerticalPitch._6_7 -
                        FrontVerticalPitch._7_8;

                    double rearPitch =
                        Header62.Y_Location -
                        Header62.Xtop -
                        RearVerticalPitch._1_2 -
                        RearVerticalPitch._2_3 -
                        RearVerticalPitch._3_4 -
                        RearVerticalPitch._4_5 -
                        RearVerticalPitch._5_6 -
                        RearVerticalPitch._6_7 -
                        RearVerticalPitch._7_8;

                    return (frontPitch - rearPitch) / (TubeLength / 12);
                }
            }
            public double Row9
            {
                get
                {
                    double frontPitch =
                        Header61.Y_Location -
                        Header61.Xtop -
                        FrontVerticalPitch._1_2 -
                        FrontVerticalPitch._2_3 -
                        FrontVerticalPitch._3_4 -
                        FrontVerticalPitch._4_5 -
                        FrontVerticalPitch._5_6 -
                        FrontVerticalPitch._6_7 -
                        FrontVerticalPitch._7_8 -
                        FrontVerticalPitch._8_9;

                    double rearPitch =
                        Header62.Y_Location -
                        Header62.Xtop -
                        RearVerticalPitch._1_2 -
                        RearVerticalPitch._2_3 -
                        RearVerticalPitch._3_4 -
                        RearVerticalPitch._4_5 -
                        RearVerticalPitch._5_6 -
                        RearVerticalPitch._6_7 -
                        RearVerticalPitch._7_8 -
                        RearVerticalPitch._8_9;

                    return (frontPitch - rearPitch) / (TubeLength / 12);
                }
            }
            public double Row10
            {
                get
                {
                    double frontPitch =
                        Header61.Y_Location -
                        Header61.Xtop -
                        FrontVerticalPitch._1_2 -
                        FrontVerticalPitch._2_3 -
                        FrontVerticalPitch._3_4 -
                        FrontVerticalPitch._4_5 -
                        FrontVerticalPitch._5_6 -
                        FrontVerticalPitch._6_7 -
                        FrontVerticalPitch._7_8 -
                        FrontVerticalPitch._8_9 -
                        FrontVerticalPitch._9_10;

                    double rearPitch =
                        Header62.Y_Location -
                        Header62.Xtop -
                        RearVerticalPitch._1_2 -
                        RearVerticalPitch._2_3 -
                        RearVerticalPitch._3_4 -
                        RearVerticalPitch._4_5 -
                        RearVerticalPitch._5_6 -
                        RearVerticalPitch._6_7 -
                        RearVerticalPitch._7_8 -
                        RearVerticalPitch._8_9 -
                        RearVerticalPitch._9_10;

                    return (frontPitch - rearPitch) / (TubeLength / 12);
                }
            }

        }
        static public FrontVerticalPitches FrontVerticalPitch => new FrontVerticalPitches();
        public class FrontVerticalPitches
        {
            public double _1_2
            {
                get => Default.FrontVerticalPitch_1_2;
                set => Default.FrontVerticalPitch_1_2 = value;
            }
            public double _2_3
            {
                get => Default.FrontVerticalPitch_2_3;
                set => Default.FrontVerticalPitch_2_3 = value;
            }
            public double _3_4
            {
                get => Default.FrontVerticalPitch_3_4;
                set => Default.FrontVerticalPitch_3_4 = value;
            }
            public double _4_5
            {
                get => Default.FrontVerticalPitch_4_5;
                set => Default.FrontVerticalPitch_4_5 = value;
            }
            public double _5_6
            {
                get => Default.FrontVerticalPitch_5_6;
                set => Default.FrontVerticalPitch_5_6 = value;
            }
            public double _6_7
            {
                get => Default.FrontVerticalPitch_6_7;
                set => Default.FrontVerticalPitch_6_7 = value;
            }
            public double _7_8
            {
                get => Default.FrontVerticalPitch_7_8;
                set => Default.FrontVerticalPitch_7_8 = value;
            }
            public double _8_9
            {
                get => Default.FrontVerticalPitch_8_9;
                set => Default.FrontVerticalPitch_8_9 = value;
            }
            public double _9_10
            {
                get => Default.FrontVerticalPitch_9_10;
                set => Default.FrontVerticalPitch_9_10 = value;
            }
        }
        static public RearVerticalPitches RearVerticalPitch => new RearVerticalPitches();
        public class RearVerticalPitches
        {
            public double _1_2
            {
                get => Default.RearVerticalPitch_1_2;
                set => Default.RearVerticalPitch_1_2 = value;
            }
            public double _2_3
            {
                get => Default.RearVerticalPitch_2_3;
                set => Default.RearVerticalPitch_2_3 = value;
            }
            public double _3_4
            {
                get => Default.RearVerticalPitch_3_4;
                set => Default.RearVerticalPitch_3_4 = value;
            }
            public double _4_5
            {
                get => Default.RearVerticalPitch_4_5;
                set => Default.RearVerticalPitch_4_5 = value;
            }
            public double _5_6
            {
                get => Default.RearVerticalPitch_5_6;
                set => Default.RearVerticalPitch_5_6 = value;
            }
            public double _6_7
            {
                get => Default.RearVerticalPitch_6_7;
                set => Default.RearVerticalPitch_6_7 = value;
            }
            public double _7_8
            {
                get => Default.RearVerticalPitch_7_8;
                set => Default.RearVerticalPitch_7_8 = value;
            }
            public double _8_9
            {
                get => Default.RearVerticalPitch_8_9;
                set => Default.RearVerticalPitch_8_9 = value;
            }
            public double _9_10
            {
                get => Default.RearVerticalPitch_9_10;
                set => Default.RearVerticalPitch_9_10 = value;
            }
        }
        public static double TubeSupportSpacing_Feet
        {
            get => Default.TubeSupportSpacingFeet;
            set => Default.TubeSupportSpacingFeet = value;
        }
        public static double TubeSupportQuantity
        {
            get => Default.TubeSupportQuantity;
            set => Default.TubeSupportQuantity = value;
        }
        public static string TubeSupportSize
        {
            get => Default.TubeSupportSize;
            set => Default.TubeSupportSize = value;
        }
        public static bool Cambered
        {
            get => Default.Camber;
            set => Default.Camber = value;
        }
        public static double OffsetFromCenter
        {
            get => Default.BundleOffsetFromCenter;
            set => Default.BundleOffsetFromCenter = value;
        }

    }
}
