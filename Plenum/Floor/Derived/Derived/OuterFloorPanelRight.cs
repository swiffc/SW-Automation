using Plenum.Floor.Derived.Derived;
using static Plenum.Plenum;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum.Floor.Derived
{
    internal class OuterFloorPanelRight : OuterFloorPanel
    {
        public new static bool Enabled => OuterFloorPanel.Enabled;
        public OuterFloorPanelRight(Design callerType) : base(callerType) { }
        public override string StaticPartNo => "193";

    }
}
