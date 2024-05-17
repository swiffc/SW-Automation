using FileTools.Base;
using ModelTools;
using System.Collections.Generic;

namespace MachineryMount.DriveFrame.Children.Derived
{
    internal class StringerL : Stringer
    {
        // Constructor
        public StringerL(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }

        // Property overrides
        public override string StaticPartNo => "236L";
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tX: -DriveFrame.Width/2)
                };
            }
        }
    }
}
