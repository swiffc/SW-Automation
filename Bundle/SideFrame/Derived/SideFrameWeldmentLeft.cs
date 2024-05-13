using FileTools.Base;
using ModelTools;
using System.Collections.Generic;

// Namespace has been overridden
namespace Bundle.SideFrame.Derived.LEFT
{
    internal class SideFrameWeldmentLeft : SideFrameWeldment
    {
        // Constructor
        public SideFrameWeldmentLeft(SW_Assembly parentAssembly) : base(parentAssembly) { }

        // Property overrides
        public override string StaticPartNo => "1011W";
        public override List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {
                    _position = new List<PositionData>
                    {
                        PositionData.Create(tX: -Bundle.Width/2)
                    };
                }
                return _position;
            }
        }
    }
}
