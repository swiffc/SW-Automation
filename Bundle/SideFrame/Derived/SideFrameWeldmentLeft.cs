using FileTools.Base;
using ModelTools;
using System.Collections.Generic;

// Namespace has been overridden
namespace Bundle.SideFrame.Derived
{
    internal class SideFrameWeldmentLeft : SideFrameWeldment
    {
        // Static properties
        static public int Priority => 1;


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
