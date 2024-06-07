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
            Plenum_Design = Design.Legacy;

            InitializePlenum(Design.Legacy);

            FloorPanel.ClearBackingFields();
        }
        public Legacy(Design callerType)
        {
            Plenum_Design = callerType;
            UpdateFloor(callerType);
        }
    }
}
