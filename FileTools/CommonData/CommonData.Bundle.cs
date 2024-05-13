using ModelTools;
using System;
using System.Windows.Forms;
using static FileTools.Properties.Settings;

namespace FileTools.CommonData
{
    public partial class CommonData
    {
        // Bundle
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
    }
}
