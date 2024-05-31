using FileTools.Base;
using ModelTools;
using System.Collections.Generic;

namespace MachineryMount.DriveWeldment.Children.Derived
{
    internal class StringerR : Stringer
    {
        // Static properties
        new static public int Priority => Stringer.Priority;


        // Constructor
        public StringerR(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }

        // Property overrides
        public override string StaticPartNo => "236R";
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tZ: DriveFrame.Width/2)
                };
            }
        }
    }
}
