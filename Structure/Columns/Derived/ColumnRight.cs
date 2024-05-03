using FileTools.Base;
using ModelTools;
using System.Collections.Generic;

namespace Structure.Columns.Derived
{
<<<<<<< HEAD
    internal class ColumnRight : Column
=======
    internal class ColumnRight : FieldColumn
>>>>>>> releases/v4.0.0
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
<<<<<<< HEAD
                    PositionData.Create(tX: Width/2, tZ: Length/2),
=======
                    PositionData.Create(tX: Width/2, tZ: Length/2, rY: 180),
>>>>>>> releases/v4.0.0
                    PositionData.Create(tX: -Width/2, tZ: -Length/2),
                };
            }
        }
    }
}
