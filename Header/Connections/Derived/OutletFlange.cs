using FileTools.Base;
using FileTools.CommonData.Headers.Connections;
using static FileTools.CommonData.CommonData;

namespace HDR.Connections.Derived
{
    internal class OutletFlange : Flange
    {
        // Constructor
        public OutletFlange(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Property overrides
        public override string PartNo => Outlet.FlangePartNo == "0" ? GetType().Name : Outlet.FlangePartNo;
        protected override IConnection FLG => Outlet;
    }
}
