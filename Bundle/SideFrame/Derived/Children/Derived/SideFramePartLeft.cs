using Bundle.SideFrame.Derived.Children;
using FileTools.Base;
using ModelTools;
using System.Collections.Generic;

// Namespace has been overridden
namespace Bundle.SideFrame.Derived.LEFT
{
    internal class SideFramePartLeft : SideFramePart
    {
        // Constructor
        public SideFramePartLeft(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Property overrides
        public override string StaticPartNo => "1011";
        public override List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {
                    _position = new List<PositionData>();

                    if (ParentSubAssembly is SideFrameWeldmentLeft)
                        _position.Add(PositionData.Create());
                }
                return _position;
            }
        }

    }
}
