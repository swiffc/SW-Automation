using Bundle.SideFrame.Derived.Children;
using FileTools.Base;
using ModelTools;
using System.Collections.Generic;

// Namespace has been overridden
namespace Bundle.SideFrame.Derived.RIGHT
{
    internal class SideFramePartRight : SideFramePart
    {
        // Constructor
        public SideFramePartRight(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Property overrides
        public override string StaticPartNo => "1012";
        public override List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {
                    _position = new List<PositionData>();

                    if (ParentSubAssembly is SideFrameWeldmentRight)
                        _position.Add(PositionData.Create());
                }
                return _position;
            }
        }
    }
}
