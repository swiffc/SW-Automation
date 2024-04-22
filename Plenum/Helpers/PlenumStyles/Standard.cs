using Plenum.Floor;
using static Plenum.Plenum;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum
{
    internal class Standard : Plenum
    {
        // Constructor
        public Standard()
        {
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
