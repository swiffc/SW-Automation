using Plenum.Floor.Derived.Derived;
using static Plenum.Plenum;

namespace Plenum.Floor.Derived
{
    internal class OuterFloorPanelRight : OuterFloorPanel
    {
        public new static bool Enabled => OuterFloorPanel.Enabled;
        public OuterFloorPanelRight(CallerType callerType) : base(callerType) { }
        public override string StaticPartNo => "193";

    }
}
