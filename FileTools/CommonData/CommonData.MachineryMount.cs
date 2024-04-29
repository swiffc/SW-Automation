using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Tools.ModelTools;
using static FileTools.Properties.Settings;

namespace FileTools.CommonData
{
    public static partial class CommonData
    {
        // User inputs
        static public double MachineryMount_Height { get; set; } = 6;
        static public double MachineryMount_Width { get; set; } = 36;
        public static string MotorShaft_Orientation => Default.MotorShaft_Orientation;


        // Readonly properties
        static public double HoleClosestToEdge_To_WidthBoundary => 4.5;

    }
}
