using FileTools.Base;
using MachineryMount.DriveAssembly;
using MachineryMount.MotorMount.Children;
using ModelTools;
using System;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;
using static Tools.ModelTools;

namespace MachineryMount.DriveWeldment.Children
{
    internal class TensioningAngle : Part
    {
        // Static properties
        public static double JackScrewSpan
        {
            get
            {
                if (jackScrewSpan == null)
                {
                    throw new InvalidOperationException("JackScrewSpan has not been set.");
                }

                return jackScrewSpan.Value;
            }
            private set
            {
                jackScrewSpan = Math.Abs(value);
            }
        }
        private double X_Translation => MotorMountPart.DriveCenterToBackingExterior + JackScrewSpan;
        static public double Length
        {
            get
            {
                if (JackScrew == Config.Outside || JackScrew == Config.ShiftedOutside)
                {
                    return DriveFrame.Width + Stringer.FlangeWidth * 2 - THK * 2;
                }
                else
                {
                    return DriveFrame.Width - HangerPRC.THK * 2 - WeldClearance * 2;
                }
            }
        }
        static public double Leg => 2.5;
        static public double THK => 0.375;
        internal static Config JackScrew
        {
            get
            {
                double defaultJackScrewSpan = 8.5;

                // Frame center up to...
                double upTo_HangerPRC = Stringer.Length / 2 - HangerPRC.FlangeDepth;
                double upToAndPast_TensionAngle = MotorMountPart.DriveCenterToBackingExterior + defaultJackScrewSpan + Leg;
                double upToAndPast_MotorMountPart = MotorMountPart.DriveCenterToBackingExterior;

                // + is no overlap
                // - is overlap
                double overlap = upTo_HangerPRC - upToAndPast_TensionAngle;

                double theoreticalMaxJackScrewSpan = (Stringer.Length / 2 - HangerPRC.THK) - upToAndPast_MotorMountPart;

                if (overlap > 0)
                {
                    JackScrewSpan = defaultJackScrewSpan;
                    return Config.Outside;
                }
                else if (overlap >= -Leg / 2)
                {
                    JackScrewSpan = defaultJackScrewSpan + overlap;
                    return Config.ShiftedOutside;
                }
                else if (overlap > -Leg + THK && overlap < -Leg / 2)
                {
                    JackScrewSpan = defaultJackScrewSpan + (Leg + overlap) + THK;
                    return Config.ShiftedInside;
                }
                else if (theoreticalMaxJackScrewSpan < defaultJackScrewSpan + Leg)
                {
                    return Config.None;
                }
                else
                {
                    JackScrewSpan = defaultJackScrewSpan;
                    return Config.Inside;
                }
            }
        }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Path", Length);
        }
        protected override void Features()
        {
            if (JackScrew == Config.Outside || JackScrew == Config.ShiftedOutside)
            {
                UnsuppressFeatures("RoundCorners");
            }
            else
            {
                SuppressFeatures("RoundCorners");
            }
        }


        // Constructor
        public TensioningAngle(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Property overrides
        public override bool Enabled => JackScrew != Config.None;
        public override string StaticPartNo => "238";
        public override Shape RawMaterialShape => Shape.Angle;
        public override string SizeOrThickness => $"{Leg}x{Leg}x{THK}";
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tX: -X_Translation, tY: Stringer.Depth)
                };
            }
        }


        // Private enumerations
        internal enum Config
        {
            Outside,
            ShiftedOutside,
            ShiftedInside,
            Inside,
            None
        }


        // Backing fields
        private static double? jackScrewSpan;
    }
}
