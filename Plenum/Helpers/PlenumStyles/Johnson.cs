using Plenum.Floor;
using Plenum.JohnsonBeam;
using System;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum
{
    internal class Johnson : Plenum
    {
        // Static properties
        public static double ExtraLength
        {
            get
            {
                if (_extraLength < MinimumExtraLength)
                {
                    return MinimumExtraLength;
                }
                return _extraLength;
            }
            set
            {
                if (value < MinimumExtraLength)
                {
                    _extraLength = MinimumExtraLength;
                }
                else
                {
                    _extraLength = value;
                }
            }
        }


        // Constructor
        public Johnson()
        {
            JohnsonBeamWld.Enabled = true;
            JohnsonSidePanel.Enabled = true;
            PlenumDesign = Design.Johnson;

            InitializePlenum(Design.Johnson);

            FloorPanel.ClearBackingFields();
        }
        public Johnson(Design callerType)
        {
            PlenumDesign = callerType;
            UpdateFloor(callerType);
        }



        // Private properties
        public static double _extraLength; // user input
        private static double MinimumExtraLength = 12;
    }
}
