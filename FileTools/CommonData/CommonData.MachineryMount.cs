using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Tools.ModelTools;
using static FileTools.Properties.Settings;
using ModelTools;

namespace FileTools.CommonData
{
    public static partial class CommonData
    {
        // Machinery Mount
        static public double HoleClosestToEdge_To_WidthBoundary => 4.5;
        static public double Stringer_Depth
        {
            get
            {
                int startIndex = Stringer_Size.IndexOf('C') + 1;
                int length = Stringer_Size.IndexOf('x') - startIndex;
                string numberStr = Stringer_Size.Substring(startIndex, length);
                double number = double.Parse(numberStr);
                return number;
            }
        }


        // User interface inputs
        public static double MachineryMount_Width
        {
            get { return Default.MachineryMount_Width; }
            set
            {
                int[] approvedValues = { 24, 30, 36, 42, 52 };
                if (value < approvedValues.First() || value > approvedValues.Last())
                {
                    Default.MachineryMount_Width = value;
                }
                else // Round up to the nearest approved value
                {
                    var closest = approvedValues.FirstOrDefault(v => v >= value);
                    Default.MachineryMount_Width = closest;
                }
            }
        }

        public static bool BeltDrive
        {
            get { return Default.IsBeltDrive; }
            set
            {
                if (value) // BeltDrive is true
                {
                    Default.IsBeltDrive = value;
                    GearDrive = false;
                    DirectDrive = false;
                }
                else if (!GearDrive && !DirectDrive) // BeltDrive is false and GearDrive and DirectDrive are also false
                {
                    throw new InvalidOperationException("BeltDrive, GearDrive, and DirectDrive cannot all be false");
                }
                else // BeltDrive is false and either GearDrive or DirectDrive is true
                {
                    Default.IsBeltDrive = value;
                }
            }
        }
        public static bool GearDrive
        {
            get { return Default.IsGearDrive; }
            set
            {
                if (value) // GearDrive is true
                {
                    Default.IsGearDrive = value;
                    BeltDrive = false;
                    DirectDrive = false;
                }
                else if (!BeltDrive && !DirectDrive) // GearDrive is false and BeltDrive and DirectDrive are also false
                {
                    throw new InvalidOperationException("BeltDrive, GearDrive, and DirectDrive cannot all be false");
                }
                else // GearDrive is false and either BeltDrive or DirectDrive is true
                {
                    Default.IsGearDrive = value;
                }
            }
        }
        public static bool DirectDrive
        {
            get { return Default.IsDirectDrive; }
            set
            {
                if (value) // DirectDrive is true
                {
                    Default.IsDirectDrive = value;
                    BeltDrive = false;
                    GearDrive = false;
                }
                else if (!BeltDrive && !GearDrive) // DirectDrive is false and BeltDrive and GearDrive are also false
                {
                    throw new InvalidOperationException("BeltDrive, GearDrive, and DirectDrive cannot all be false");
                }
                else // DirectDrive is false and either BeltDrive or GearDrive is true
                {
                    Default.IsDirectDrive = value;
                }
            }
        }

        public static bool ForcedDraft
        {
            get { return Default.IsForced; }
            set
            {
                if (value) // Forced is true
                {
                    Default.IsForced = value;
                    Induced = false;
                }
                else if (!Induced) // Forced is false and Induced is also false
                {
                    throw new InvalidOperationException("Forced and Induced cannot both be false");
                }
                else // Forced is false and Induced is true
                {
                    Default.IsForced = value;
                }
            }
        }
        public static bool Induced
        {
            get { return Default.IsInduced; }
            set
            {
                if (value) // Induced is true
                {
                    Default.IsInduced = value;
                    ForcedDraft = false;
                }
                else if (!ForcedDraft) // Induced is false and Forced is also false
                {
                    throw new InvalidOperationException("Forced and Induced cannot both be false");
                }
                else // Induced is false and Forced is true
                {
                    Default.IsInduced = value;
                }
            }
        }

        public static bool MotorShaftUp
        {
            get { return Default.IsMotorShaftUp; }
            set
            {
                if (value) // MotorShaftUp is true
                {
                    Default.IsMotorShaftUp = value;
                    MotorShaftDown = false;
                }
                else if (!MotorShaftDown) // MotorShaftUp is false and MotorShaftDown is also false
                {
                    throw new InvalidOperationException("MotorShaftUp and MotorShaftDown cannot both be false");
                }
                else // MotorShaftUp is false and MotorShaftDown is true
                {
                    Default.IsMotorShaftUp = value;
                }
            }
        }
        public static bool MotorShaftDown
        {
            get { return Default.IsMotorShaftDown; }
            set
            {
                if (value) // MotorShaftDown is true
                {
                    Default.IsMotorShaftDown = value;
                    MotorShaftUp = false;
                }
                else if (!MotorShaftUp) // MotorShaftDown is false and MotorShaftUp is also false
                {
                    throw new InvalidOperationException("MotorShaftUp and MotorShaftDown cannot both be false");
                }
                else // MotorShaftDown is false and MotorShaftUp is true
                {
                    Default.IsMotorShaftDown = value;
                }
            }
        }

        static public int MotorFrameSize
        {
            get { return Default.MotorFrameSize; }
            set
            {
                int[] approvedValues = { 182, 184, 213, 215, 254, 256, 284, 286, 324, 326, 364, 365, 404, 405 };
                if (value > approvedValues.First() || value < approvedValues.Last())
                    Default.MotorFrameSize = value;
                else
                    throw new NotImplementedException();
            }
        }
        static public double FanShaft_Diameter
        {
            get { return Default.FanShaft_Diameter; }
            set
            {
                if (!FanShaftData.ContainsKey(value))
                {
                    throw new InvalidOperationException($"Invalid value for FanShaft_Diameter. Supported values are: {string.Join(", ", FanShaftData.Keys)}");
                }
                Default.FanShaft_Diameter = value;
            }
        }
        static public double MotorCenter_To_FanCenter
        {
            get { return Default.Motor_CenterToCenter; }
            set { Default.Motor_CenterToCenter = value; }
        }
        private static double _defaultMotorRise = 1.5;
        public static double MotorRise
        {
            get
            {
                double Y_Location = _defaultMotorRise;
                var motor = MotorFrame[MotorFrameSize];

                double motorSpanInsideStringers = motor.BA + motor.NW - Y_Location;

                if (motorSpanInsideStringers > Stringer_Depth - MinimumShaftDistanceToDriveBottom)
                {
                    Y_Location += motorSpanInsideStringers - (Stringer_Depth - MinimumShaftDistanceToDriveBottom);
                }

                return Y_Location;
            }
        }
        public static double MinimumShaftDistanceToDriveBottom => 1.5;
        public static string Vibration_Sensor
        {
            get { return Default.Vibration_Sensor; }
            set { Default.Vibration_Sensor = value; }
        }
        public static double MachineryMount_Length
        {
            get { return Default.MachineryMount_Length; }
            set { Default.MachineryMount_Length = value; }
        }
        public static double MotorShift
        {
            get { return Default.MotorShift; }
            set { Default.MotorShift = value; }
        }





        // User interface overrides
        static public bool Lock_StringerSize { get; set; } = false;
        public static string Stringer_Size
        {
            get
            {
                bool dependenciesHaveChanged =
                    _plenumWidth != Plenum_Width ||
                    _totalUnitWeight != TotalUnitWeight ||
                    _fanCount != Fan_Count;

                if (dependenciesHaveChanged && !Lock_StringerSize)
                {
                    _plenumWidth = Plenum_Width;
                    _totalUnitWeight = TotalUnitWeight;
                    _fanCount = Fan_Count;

                    _size = SetDefault_StringerSize();
                }

                return _size;
            }
            set
            {
                string valueFormatted = value.ToLower();
                valueFormatted = char.ToUpper(valueFormatted[0]) + valueFormatted.Substring(1);

                if (!SupportedStringerSizes.Contains(valueFormatted))
                {
                    throw new InvalidOperationException($"\"{value}\" is not a supported value for Stringer_Size");
                }

                _size = valueFormatted;
            }
        }
        static public bool Lock_MachineryMountHeight { get; set; } = false;
        static public double MachineryMount_Height
        {
            get
            {
                bool dependentsHaveChanged =
                    _ringDepth != FanRing_Depth ||
                    _stringerDepth != Stringer_Depth ||
                    _motorFrameSize != MotorFrameSize;

                if (dependentsHaveChanged && !Lock_MachineryMountHeight)
                {
                    _ringDepth = FanRing_Depth;
                    _stringerDepth = Stringer_Depth;
                    _motorFrameSize = MotorFrameSize;

                    _machineryMountHeight = SetDefault_MachineryMountHeight();
                }

                return _machineryMountHeight;
            }
            set
            {
                _machineryMountHeight = value;
            }
        }


        // Private methods
        private static string SetDefault_StringerSize()
        {
            double value = _totalUnitWeight / _fanCount;
            if (_plenumWidth <= 108 && FanShaft_Diameter <= 2.25)
            {
                return "C8x11.5";
            }
            else if (_plenumWidth > 108 && _plenumWidth <= 132)
            {
                if (value >= 25000) return "C10x20";
                else return "C10x15.3";
            }
            else if (_plenumWidth > 132 && _plenumWidth <= 144)
            {
                if (value >= 24000) return "C12x20.7";
                else if (value >= 20000 && value < 24000) return "C10x20";
                else return "C10x15.3";
            }
            else if (_plenumWidth > 144 && _plenumWidth <= 156)
            {
                if (value >= 32000) return "C15x33.9";
                else if (value >= 27500 && value < 32000) return "C12x25";
                else if (value >= 18500 && value < 27500) return "C12x20.7";
                else if (value >= 15000 && value < 18500) return "C10x20";
                else return "C10x15.3";
            }
            else if (_plenumWidth > 156 && _plenumWidth <= 168)
            {
                if (value >= 22000) return "C15x33.9";
                else if (value >= 19000 && value < 22000) return "C12x25";
                else return "C12x20.7";
            }
            else
            {
                return MotorFrameSize <= 286 ? "C10x15.3" : "C12x20.7";
            }
        }
        private static double SetDefault_MachineryMountHeight()
        {
            var motor = MotorFrame[MotorFrameSize];
            double height = motor.C - motor.NW - motor.BA + 6 + FanRing_Depth + Stringer_Depth;

            if (height - FanRing_Depth < 22)
                height = FanRing_Depth + 22;

            height = Math.Ceiling(height / 0.125) * 0.125;

            height += MotorRise - _defaultMotorRise;

            return height;
        }


        // Backing fields
        private static string _size;
        private static double _totalUnitWeight;
        private static double _fanCount;
        private static double _machineryMountHeight;
        private static double _ringDepth;
        private static double _stringerDepth;
        private static double _motorFrameSize;
        private static double _plenumWidth;


        // Readonly properties
        public static readonly string[] SupportedStringerSizes =
        {
            "C3x4.1", "C3x5", "C3x6",
            "C4x5.4", "C4x7.25",
            "C5x6.7", "C5x9",
            "C6x8.2","C6x10.5", "C6x13",
            "C7x9.8", "C7x12.25", "C7x14.75",
            "C8x11.5", "C8x13.75", "C8x18.75",
            "C9x13.4", "C9x15", "C9x20",
            "C10x15.3", "C10x20", "C10x25","C10x30",
            "C12x20.7", "C12x25", "C12x30",
            "C15x33.9.9", "C15x40", "C15x50"
        };


        // Dictionaries
        public static Dictionary<double, (double GrooveDepth, double GrooveWidth, double KeyWidth)> FanShaftData = new Dictionary<double, (double, double, double)>
        {
        //  Diameter        GrooveDepth     GrooveWidth   KeyWidth
            { 1.9375,   (   0.056,          0.068,        0.500      ) },
            { 2.4375,   (   0.070,          0.086,        0.625      ) },
            { 2.9375,   (   0.080,          0.103,        0.750      ) },
        };
        public static Dictionary<double, (double O, double D, double BA, double C, double NW, double E, double F, double U, double AB, double H)> MotorFrame =
            new Dictionary<double, (double O, double D, double BA, double C, double NW, double E, double F, double U, double AB, double H)>
            {
            //    Size       O,          D,          BA,         C,         NW,         E,           F,         U,           AB         H
                { 182,      (8.8750,     4.5000,     2.7500,     14.8125,   2.7500,     3.7500,      4.5000,    1.1250,      7.3750,    0.4375) },
                { 184,      (8.8750,     4.5000,     2.7500,     16.2500,   2.7500,     3.7500,      5.5000,    1.1250,      7.3750,    0.4375) },
                { 213,      (10.6250,    5.2500,     5.2500,     20.3125,   3.3750,     4.2500,      5.5000,    1.3750,      9.0625,    0.4375) },
                { 215,      (10.6250,    5.2500,     5.2500,     20.3125,   3.3750,     4.2500,      7.0000,    1.3750,      9.0625,    0.4375) },
                { 254,      (12.6250,    6.2500,     4.2500,     25.8125,   4.0000,     5.0000,      8.2500,    1.6250,      9.9375,    0.5625) },
                { 256,      (12.6250,    6.2500,     4.2500,     25.8125,   4.0000,     5.0000,     10.0000,    1.6250,      9.9375,    0.5625) },
                { 284,      (14.1875,    7.0000,     4.7500,     29.4375,   4.6250,     5.5000,      9.5000,    1.8750,     13.4375,    0.5625) },
                { 286,      (14.1817,    7.0000,     4.7500,     29.4375,   4.6250,     5.5000,     11.0000,    1.8750,     13.4375,    0.5625) },
                { 324,      (15.9375,    8.0000,     5.2500,     32.1250,   5.2500,     6.2500,     10.5000,    2.1250,     15.7500,    0.6875) },
                { 326,      (15.9375,    8.0000,     5.2500,     32.1250,   5.2500,     6.2500,     12.0000,    2.1250,     15.7500,    0.6875) },
                { 364,      (17.8125,    9.0000,     5.8750,     35.6250,   5.8750,     7.0000,     11.2500,    2.3750,     17.6875,    0.6875) },
                { 365,      (17.8125,    9.0000,     5.8750,     35.6250,   5.8750,     7.0000,     12.2500,    2.3750,     17.6875,    0.6875) },
                { 404,      (19.9375,   10.0000,     6.2500,     39.5000,   7.2500,     8.0000,     12.2500,    2.8750,     17.5000,    0.8125) },
                { 405,      (19.9375,   10.0000,     6.2500,     39.5000,   7.2500,     8.0000,     13.7500,    2.8750,     17.5000,    0.8125) },
            };

    }
}
