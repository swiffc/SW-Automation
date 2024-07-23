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
    internal class OutletFlange : Flange
    {
        // Constructor
        public OutletFlange(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Property overrides
        public override string PartNo => Outlet.FlangePartNo == "0" ? GetType().Name : Outlet.FlangePartNo;
        protected override IConnection FLG => Outlet;






    }
}
