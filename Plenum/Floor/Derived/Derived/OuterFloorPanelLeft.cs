using Plenum.Floor.Derived.Derived;
using static Plenum.Plenum;

namespace Plenum.Floor.Derived
{
    internal class OuterFloorPanelLeft : OuterFloorPanel
    {
        public new static bool Enabled => OuterFloorPanel.Enabled;
        public OuterFloorPanelLeft(CallerType callerType) : base(callerType) { }
        public override string StaticPartNo => "191";

    }
}
