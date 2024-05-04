using static Plenum.Plenum;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum.Floor.Derived
{
    internal class InnerFloorPanelLeft : InnerFloorPanel
    {
        public new static bool Enabled => InnerFloorPanel.Enabled;
        public InnerFloorPanelLeft(Design callerType) : base(callerType) { }
        public override string StaticPartNo => "195";

    }
}
