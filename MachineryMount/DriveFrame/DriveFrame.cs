using FileTools.Base;
using ModelTools;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;

namespace MachineryMount.DriveFrame
{
    internal class DriveFrame : SubAssembly
    {
        // Static properties
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
