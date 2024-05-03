using Plenum.Floor.Derived.Derived;
using static Plenum.Plenum;

namespace Plenum.Floor.Derived
{
    internal class InnerFloorPanelRight : InnerFloorPanel
    {
        public new static bool Enabled => InnerFloorPanel.Enabled;
        public InnerFloorPanelRight(CallerType callerType) : base(callerType) { }
        public override string StaticPartNo => "197";

    }
}
