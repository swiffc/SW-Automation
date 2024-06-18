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
                get => Default.SlopePerFoot_Row1; 
                set => Default.SlopePerFoot_Row1 = value; 
            }
            public double Row2 
            { 
                get => Default.SlopePerFoot_Row2; 
                set => Default.SlopePerFoot_Row2 = value; 
            }
            public double Row3 
            { 
                get => Default.SlopePerFoot_Row3; 
                set => Default.SlopePerFoot_Row3 = value; 
            }
            public double Row4 
            { 
                get => Default.SlopePerFoot_Row4; 
                set => Default.SlopePerFoot_Row4 = value; 
            }
            public double Row5 
            { 
                get => Default.SlopePerFoot_Row5; 
                set => Default.SlopePerFoot_Row5 = value; 
            }
            public double Row6 
            { 
                get => Default.SlopePerFoot_Row6; 
                set => Default.SlopePerFoot_Row6 = value; 
            }
            public double Row7 
            { 
                get => Default.SlopePerFoot_Row7; 
                set => Default.SlopePerFoot_Row7 = value; 
            }
            public double Row8 
            { 
                get => Default.SlopePerFoot_Row8; 
                set => Default.SlopePerFoot_Row8 = value; 
            }
            public double Row9 
            { 
                get => Default.SlopePerFoot_Row9; 
                set => Default.SlopePerFoot_Row9 = value; 
            }
            public double Row10 
            { 
                get => Default.SlopePerFoot_Row10;
                set => Default.SlopePerFoot_Row10 = value; 
            }
        }

        static public VerticalPitchs VerticalPitch => new VerticalPitchs();
        public class VerticalPitchs
        {
            public double _1_2
            {
                get => Default.VerticalPitch_1_2;
                set => Default.VerticalPitch_1_2 = value;
            }
            public double _2_3
            {
                get => Default.VerticalPitch_2_3;
                set => Default.VerticalPitch_2_3 = value;
            }
            public double _3_4
            {
                get => Default.VerticalPitch_3_4;
                set => Default.VerticalPitch_3_4 = value;
            }
            public double _4_5
            {
                get => Default.VerticalPitch_4_5;
                set => Default.VerticalPitch_4_5 = value;
            }
            public double _5_6
            {
                get => Default.VerticalPitch_5_6;
                set => Default.VerticalPitch_5_6 = value;
            }
            public double _6_7
            {
                get => Default.VerticalPitch_6_7;
                set => Default.VerticalPitch_6_7 = value;
            }
            public double _7_8
            {
                get => Default.VerticalPitch_7_8;
                set => Default.VerticalPitch_7_8 = value;
            }
            public double _8_9
            {
                get => Default.VerticalPitch_8_9;
                set => Default.VerticalPitch_8_9 = value;
            }
            public double _9_10
            {
                get => Default.VerticalPitch_9_10;
                set => Default.VerticalPitch_9_10 = value;
            }
        }
    }
}
