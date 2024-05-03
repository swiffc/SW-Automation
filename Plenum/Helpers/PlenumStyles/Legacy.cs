using Plenum.Floor;

namespace Plenum
{
    internal class Legacy : Plenum
    {
        // Constructor
        public Legacy()
        {
            FlatSeal.Enabled = true;
            StaticCaller = CallerType.Legacy;

            InitializePlenum(CallerType.Legacy);

            FloorPanel.ClearBackingFields();
        }
        public Legacy(CallerType callerType)
        {
            StaticCaller = callerType;
            UpdateFloor(callerType);
        }
    }
}
