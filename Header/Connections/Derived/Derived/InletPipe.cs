using FileTools.Base;
using FileTools.CommonData.Headers.Connections;
using static FileTools.CommonData.CommonData;

namespace HDR.Connections.Derived.Derived
{
    internal class InletPipe : Pipe
    {
        // Constructor
        public InletPipe(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Property overrides
        public override string PartNo => "InletPipe";
        protected override IConnection Ext => Inlet;
    }
}
