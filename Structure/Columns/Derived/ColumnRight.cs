using FileTools.Base;
using ModelTools;
using System.Collections.Generic;

namespace Structure.Columns.Derived
{
    internal class ColumnRight : FieldColumn
    {
        // Constructor
        public ColumnRight(SW_Assembly parentAssembly) : base(parentAssembly) { }


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => "106";
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tX: Width/2, tZ: Length/2, rY: 180),
                    PositionData.Create(tX: -Width/2, tZ: -Length/2),
                };
            }
        }
    }
}
