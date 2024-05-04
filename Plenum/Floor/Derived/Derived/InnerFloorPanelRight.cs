using Plenum.Floor.Derived.Derived;
using static Plenum.Plenum;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum.Floor.Derived
{
    internal class InnerFloorPanelRight : InnerFloorPanel
    {
        public new static bool Enabled => InnerFloorPanel.Enabled;
        public InnerFloorPanelRight(Design callerType) : base(callerType) { }
        public override string StaticPartNo => "197";

    }
}
