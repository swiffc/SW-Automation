using Plenum.Floor;
using static Plenum.Plenum;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;
using static FileTools.Properties.Settings;

namespace Plenum
{
    public class Standard : Plenum
    {
        // Constructor
        public Standard()
        {
            Default.Beams_AreRotated = false;
            PlenumDesign = Design.Standard;

            InitializePlenum(Design.Standard);

            FloorPanel.ClearBackingFields();
        }
        public Standard(Design callerType)
        {
            PlenumDesign = callerType;
            UpdateFloor(callerType);
        }
    }
}
