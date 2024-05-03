using static Plenum.Plenum;

namespace Plenum.Floor.Derived
{
    internal class InnerFloorPanelLeft : InnerFloorPanel
    {
        public new static bool Enabled => InnerFloorPanel.Enabled;
        public InnerFloorPanelLeft(CallerType callerType) : base(callerType) { }
        public override string StaticPartNo => "195";

    }
}
