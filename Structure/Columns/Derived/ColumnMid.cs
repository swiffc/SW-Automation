using FileTools.Base;
using ModelTools;
using System.Collections.Generic;

namespace Structure.Columns.Derived
{
<<<<<<< HEAD
    internal class ColumnMid : Column
=======
    internal class ColumnMid : FieldColumn
>>>>>>> releases/v4.0.0
    {
        // Constructor
        public ColumnMid(SW_Assembly parentAssembly) : base(parentAssembly) { }


        // Property overrides
        public override bool Enabled => MidColumns;
        public override string StaticPartNo => "111";
        public override List<PositionData> Position
        {
            get
            {
                var pos = new List<PositionData>();
                double zTranslation = Length / 2 - Length / FanCount;

                for (int i = 0; i < FanCount - 1; i++)
                {
                    pos.Add(PositionData.Create(tX: -Width / 2, tZ: zTranslation));
<<<<<<< HEAD
                    pos.Add(PositionData.Create(tX: Width / 2, tZ: zTranslation));
=======
                    pos.Add(PositionData.Create(tX: Width / 2, tZ: zTranslation, rY: 180));
>>>>>>> releases/v4.0.0
                    zTranslation -= Length / FanCount;
                }

                return pos;
            }
        }
    }
}
