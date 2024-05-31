using FileTools.Base;
using ModelTools;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;

namespace MachineryMount.DriveWeldment
{
    internal class DriveFrame : SubAssembly
    {
        // Static properties
        static public int Priority => 1;
        public static double Width
        {
            get { return MachineryMount_Width; }
            set { MachineryMount_Width = value; }
        }
        public static double Height
        {
            get { return MachineryMount_Height; }
            set { MachineryMount_Height = value; }
        }
        static public double TowerHeight
        {
            get
            {
                return Height - FanRing_Depth - 2.25;
            }
        }


        // Constructor
        public DriveFrame(SW_Assembly parentAssembly) : base(parentAssembly) { }


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => "236";
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
