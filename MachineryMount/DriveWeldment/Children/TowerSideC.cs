using FileTools.Base;
using MachineryMount.DriveAssembly;
using ModelTools;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;
using static MachineryMount.DriveAssembly.VibrationSensor;

namespace MachineryMount.DriveWeldment.Children
{
    internal class TowerSideC : Part
    {
        // Static properties
        static public string Size
        {
            get
            {
                return FanShaft_Diameter <= 2.25 ? "C8x11.5" : "C10x15.3";
            }
        }
        static public double WebTHK => 0.25; // approx
        static public double Length => DriveFrame.TowerHeight - Stringer.Depth;


        // Constructor
        public TowerSideC(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Path", Length);

            if (VibrationSensor.IsEnabled())
            {
                EditDimension("Diameter", "sk:VibrationSensorHole", isMetrix ? 0.25 : 0.375);
                EditDimension("Y_Upper", "sk:VibrationSensorHole", isMetrix ? Length / 2 + 0.75 : Length / 2 + 2.625);
                EditDimension("Y_Lower", "sk:VibrationSensorHole", isMetrix ? Length / 2 - 0.375 : Length / 2 - 2.625);
                EditDimension("X_Upper", "sk:VibrationSensorHole", isMetrix ? 0.001 : 3);
                EditDimension("X_Lower", "sk:VibrationSensorHole", isMetrix ? 1.299038106 : 3);
            }
        }
        protected override void Features()
        {
            EditFeature_StructuralMemberSize(Size);

            var features = new string[] { "VibrationSensorHole", "VibrationSensorHoles" };
            if (VibrationSensor.IsEnabled())
                UnsuppressFeatures(features);
            else
                SuppressFeatures(features);
        }


        // Property overrides
        public override bool Enabled => ForcedDraft;
        public override string StaticPartNo => "226P";
        public override Shape RawMaterialShape => Shape.Channel;
        public override string SizeOrThickness => Size;
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tY: Stringer.Depth, tZ: DriveFrame.Width/2),
                    PositionData.Create(tY: Stringer.Depth, tZ: -DriveFrame.Width/2, rY: 180)
                };
            }
        }
    }
}
