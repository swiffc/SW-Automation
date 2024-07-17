using FileTools.Base;
using FileTools.CommonData.Headers.Connections;
using static FileTools.CommonData.CommonData;

namespace HDR.Connections.Derived
{
    internal class InletFlange : Flange
    {
        // Constructor
        public InletFlange(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Property overrides
        public override string PartNo => Inlet.FlangePartNo == "0" ? GetType().Name : Inlet.FlangePartNo;
        protected override IConnection FLG => Inlet;
    }
}
