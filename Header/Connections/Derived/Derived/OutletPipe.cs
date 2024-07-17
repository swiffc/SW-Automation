using FileTools.Base;
using FileTools.CommonData.Headers.Connections;
using static FileTools.CommonData.CommonData;

namespace HDR.Connections.Derived.Derived
{
    internal class OutletPipe : Pipe
    {
        // Constructor
        public OutletPipe(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Property overrides
        public override string PartNo => Outlet.ExtensionPartNo == "0" ? GetType().Name : Outlet.ExtensionPartNo;
        protected override IConnection Ext => Outlet;
    }
}
