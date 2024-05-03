using FileTools.Base;
using ModelTools;
using System.Collections.Generic;

namespace Structure.Columns.Derived
{
<<<<<<< HEAD
    internal class ColumnLeft : Column
=======
    internal class ColumnLeft : FieldColumn
>>>>>>> releases/v4.0.0
    {
        // Constructor
        public ColumnLeft(SW_Assembly parentAssembly) : base(parentAssembly) { }


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => "101";
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tX: -Width/2, tZ: Length/2),
<<<<<<< HEAD
                    PositionData.Create(tX: Width/2, tZ: -Length/2),
=======
                    PositionData.Create(tX: Width/2, tZ: -Length/2, rY: 180),
>>>>>>> releases/v4.0.0
                };
            }
        }
    }
}
