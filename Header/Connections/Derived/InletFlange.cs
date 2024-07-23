using FileTools.Base;
using FileTools.CommonData.Headers.Connections;
using HDR.Box;
using HDR.Connections.Derived;
using ModelTools;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;
using static HDR.HeaderBase;


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
