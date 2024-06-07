using FileTools.Base;
using MachineryMount.DriveAssembly;
using ModelTools;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using static FileTools.StaticFileTools;

namespace MachineryMount.DriveAssembly
{
    internal abstract class Bearing : Part
    {
        // Static properties
        static public (double Height, double HoleDiameter, double HoleToHoleSpacing) Top => RollerBearingRequired ? BearingDims[SelectRollerBearing()] : BearingDims[SelectBallBearing()];
        static public (double Height, double HoleDiameter, double HoleToHoleSpacing) Bottom => BearingDims[SelectBallBearing()];


        // Constructor
        protected Bearing(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Protected properties
        protected static bool RollerBearingRequired
        {
            get
            {
                if (FanDiameter_Feet >= 9 || Motor.Frame >= 256)
                    return true;
                else return false;
            }
        }


        // Protected methods
        protected static string SelectRollerBearing()
        {
            switch (FanShaft_Diameter)
            {
                case 1.9375:
                    return "NullBearing";
                case 2.4375:
                    return "50084";
                case 2.9375:
                    return "50110";
                default:
                    throw new NotImplementedException();
            }
        }
        protected static string SelectBallBearing()
        {
            switch (FanShaft_Diameter)
            {
                case 1.9375:
                    return "69211";
                case 2.4375:
                    return "50098";
                case 2.9375:
                    return "50100";
                default:
                    throw new NotImplementedException();
            }
        }


        // Property overrides
        public override Shape RawMaterialShape => 0;
        public override string SizeOrThickness => "";


        // Dictionaries
        public static Dictionary<string, (double Height, double HoleDiameter, double HoleToHoleSpacing)> BearingDims =
                  new Dictionary<string, (double Height, double HoleDiameter, double HoleToHoleSpacing)>
                  {
                  //     P/N             Height         HoleDiameter     HoleToHoleSpacing
                      { "NullBearing",  (1.00000,       0.12500,         5.00000) },
                      { "50084",        (4.56250,       0.75000,         5.37500) },
                      { "50110",        (4.68750,       0.87500,         6.00000) },

                      { "69211",        (2.15625,       0.68750,         5.12500) },
                      { "50098",        (2.75000,       0.68750,         5.87500) },
                      { "50100",        (3.09375,       0.87500,         6.00000) }
                  };
    }
}
