using FileTools.Base;

namespace Bundle.SideFrame
{
    internal abstract class SideFrameWeldment : SubAssembly
    {
        // Constructor
        public SideFrameWeldment(SW_Assembly parentAssembly) : base(parentAssembly) { }


        // Property overrides
        public override bool Enabled => true;
    }
}
