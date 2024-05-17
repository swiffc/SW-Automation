using FileTools.Base;
using ModelTools;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;

namespace Structure.Columns.Derived
{
    internal class ColumnMid : FieldColumn
    {
        // Constructor
        public ColumnMid(SW_Assembly parentAssembly) : base(parentAssembly) { }


        // Property overrides
        public override bool Enabled => Mid_Columns;
        public override string StaticPartNo => "111";
        public override List<PositionData> Position
        {
            get
            {
                var pos = new List<PositionData>();
                double zTranslation = Plenum_Length / 2 - Plenum_Length / Fan_Count;

                for (int i = 0; i < Fan_Count - 1; i++)
                {
                    pos.Add(PositionData.Create(tX: -Plenum_Width / 2, tZ: zTranslation));
                    pos.Add(PositionData.Create(tX: Plenum_Width / 2, tZ: zTranslation, rY: 180));
                    zTranslation -= Plenum_Length / Fan_Count;
                }

                return pos;
            }
        }
    }
}
