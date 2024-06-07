using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using MachineryMount.DriveWeldment;
using MachineryMount.DriveWeldment.Children;

namespace MachineryMount.DriveAssembly
{
    internal class FanShaft : Part
    {
        // Static properties
        public static double Diameter
        {
            get { return FanShaft_Diameter; }
            set { FanShaft_Diameter = value; }
        }
        static public double Length => DriveFrame.Height - BottomReduction;


        // Constructor
        public FanShaft(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Diameter", "sk:Profile", Diameter);
            EditDimension("Length", "sk:Path", Length);

            EditDimension("Depth", "sk:SnapRingGrooves", FanShaftData[Diameter].GrooveDepth);
            EditDimension("Width", "sk:SnapRingGrooves", FanShaftData[Diameter].GrooveWidth);
            EditDimension("TopOfTower", "sk:SnapRingGrooves", DriveFrame.TowerHeight + Bearing.Top.Height - BottomReduction);

            EditDimension("Depth", "sk:Keyways", FanShaftData[Diameter].KeyWidth / 2);
            EditDimension("Width", "Keyways", FanShaftData[Diameter].KeyWidth);
        }


        // Private properties
        private static double BottomReduction => Stringer.Depth - (Motor.Dim.BA + Motor.Dim.NW - MotorRise);


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => "FS";
        public override Shape RawMaterialShape => Shape.BarStock;
        public override string SizeOrThickness => Diameter.ToString();
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tY: BottomReduction)
                };
            }
        }
    }
}
