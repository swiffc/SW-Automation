using FileTools.Base;
using ModelTools;
using System.Collections.Generic;

// Namespace has been overridden
namespace Bundle.SideFrame.Derived.RIGHT
{
    internal class SideFrameWeldmentRight : SideFrameWeldment
    {
        // Constructor
        public SideFrameWeldmentRight(SW_Assembly parentAssembly) : base(parentAssembly) { }

        // Property overrides
        public override string StaticPartNo => "1012W";
        public override List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {
                    _position = new List<PositionData>
                    {
                        PositionData.Create(tX: Bundle.Width/2, tY: 180)
                    };
                }
                return _position;
            }
        }
    }
}
