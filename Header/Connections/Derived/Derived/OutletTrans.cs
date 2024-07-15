using FileTools.Base;
using FileTools.CommonData.Headers.Connections;
using static FileTools.CommonData.CommonData;

namespace HDR.Connections.Derived.Derived
{
    internal class OutletTrans : Transition
    {
        // Constructor
        public OutletTrans(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Property overrides
        public override string PartNo => "OutletTrans";
        protected override IConnection Ext => Outlet;
    }
}
