using Plenum.Floor;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;
using static FileTools.Properties.Settings;

namespace Plenum
{
    public class Legacy : Plenum
    {
        // Constructor
        public Legacy()
        {
            Default.Beams_AreRotated = true;
            FlatSeal.Enabled = true;
            PlenumDesign = Design.Legacy;

            InitializePlenum(Design.Legacy);

            FloorPanel.ClearBackingFields();
        }
        public Legacy(Design callerType)
        {
            PlenumDesign = callerType;
            UpdateFloor(callerType);
        }
    }
}
