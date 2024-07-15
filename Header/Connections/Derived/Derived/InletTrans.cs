using FileTools.Base;
using FileTools.CommonData.Headers.Connections;
using static FileTools.CommonData.CommonData;

namespace HDR.Connections.Derived.Derived
{
    internal class InletTrans : Transition
    {
        // Constructor
        public InletTrans(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Property overrides
        public override string PartNo => "InletTrans";
        protected override IConnection Ext => Inlet;
    }
}
