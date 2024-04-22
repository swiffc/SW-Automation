using Plenum.Floor;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum
{
    internal class Legacy : Plenum
    {
        // Constructor
        public Legacy()
        {
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
