using FileTools.Base;
using MachineryMount.DriveWeldment;
using MachineryMount.DriveWeldment.Children;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;

namespace MachineryMount.DriveAssembly
{
    internal class VibrationSensor : Part
    {
        // Static properties
        public static string Sensor
        {
            get { return Vibration_Sensor; }
            set { Vibration_Sensor = value; }
        }
        static public double Y_Center => Stringer.Depth + TowerSideC.Length / 2;
        static public bool isMetrix => Sensor.ToLower().Contains("metrix");


        // Constructor
        public VibrationSensor(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Static methods
        public static bool IsEnabled()
        {
            return Info.ContainsKey(Sensor);
        }


        // Property overrides
        public override bool Enabled => IsEnabled();
        public override string StaticPartNo => Enabled ? Info[Sensor] : null;
        public override Shape RawMaterialShape => 0;
        public override string SizeOrThickness => "";
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tY: Y_Center, tZ: DriveFrame.Width/2 + TowerSideC.WebTHK),
                };
            }
        }

        public override string PartNo => StaticPartNo;
        public override string FilePath => $@"C:\AXC_VAULT\SolidWorks Common Data\Design Library\Hudson Library\Vibration Sensors\{StaticPartNo}.SLDPRT";


        // Dictionaries
        public static Dictionary<string, string> Info = new Dictionary<string, string>
        {
            { "Murphy VS-2-EX",     "51505"      },
            { "Metrix",             "30587-KIT"  },
        };
    }
}
