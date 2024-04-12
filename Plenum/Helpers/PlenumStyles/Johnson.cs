using Plenum.Floor;
using Plenum.JohnsonBeam;
using System;

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
            StaticCaller = CallerType.Johnson;

            InitializePlenum(CallerType.Johnson);

            FloorPanel.ClearBackingFields();
        }
        public Johnson(CallerType callerType)
        {
            StaticCaller = callerType;
            UpdateFloor(callerType);
        }



        // Private properties
        public static double _extraLength; // user input
        private static double MinimumExtraLength = 12;
    }
}
