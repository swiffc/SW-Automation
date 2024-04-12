using Plenum.Floor;
using static Plenum.Plenum;

namespace Plenum
{
    internal class Standard : Plenum
    {
        // Constructor
        public Standard()
        {
            StaticCaller = CallerType.Standard;

            InitializePlenum(CallerType.Standard);

            FloorPanel.ClearBackingFields();
        }
        public Standard(CallerType callerType)
        {
            StaticCaller = callerType;
            UpdateFloor(callerType);
        }
    }
}
