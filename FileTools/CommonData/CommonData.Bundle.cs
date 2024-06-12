using ModelTools;
using System;
using System.Windows.Forms;
using static FileTools.Properties.Settings;

namespace FileTools.CommonData
{
    public static partial class CommonData
    {
        // Bundle
        static public bool HeadersAreOutsideTheFrame { get; set; } = true;
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
        public static double TubeStickThru
        {
            get => Default.TubeStickThru;
            set => Default.TubeStickThru = value;
        }
        public static bool HeadersOutsideFrames
        {
            get => Default.HeadersOutsideFrames;
            set => Default.HeadersOutsideFrames = value;
        }
    }
}
