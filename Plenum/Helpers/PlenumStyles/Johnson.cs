using Plenum.Floor;
using Plenum.JohnsonBeam;
using System;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;
using static FileTools.Properties.Settings;

namespace Plenum
{
    public class Johnson : Plenum
    {
        // Constructor
        public Johnson()
        {
            Default.Beams_AreRotated = true;
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
    }
}
