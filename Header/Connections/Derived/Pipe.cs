using FileTools.Base;
using static FileTools.CommonData.CommonData;
using static HDR.HeaderBase;

namespace HDR.Connections.Derived
{
    internal abstract class Pipe : Extension
    {
        // Constructor
        protected Pipe(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Wall", "sk:Pipe", Ext.Wall);
            EditDimension("OD", "sk:Pipe", Ext.OD);
            EditDimension("Length", "sk:Pipe", Length);
        }


        // Property overrides
        public override bool Enabled =>
            ((Location == "TL" && Header == Header61) ||
            (Location == "TR" && Header == Header62) ||
            (Location == "BL" && Header == LowestLeftHeader) ||
            (Location == "BR" && Header == LowestRightHeader)) &&
            (Ext.ExtensionType == "Pipe");
        public override Shape RawMaterialShape => Shape.Pipe;
        public override string StaticPartNo => "Pipe";
    }
}
