using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using static HDR.HeaderBase;

namespace HDR.Box.Derived
{
    internal class Partition2 : Partition
    {
        // Constructor
        public Partition2(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Protected properties
        protected PositionData Position2 => PositionData.Create(tY: -GetYTranslation(
            Header.PartitionDistanceBelow2, 
            LocationBelowRowNumber2));


        // Property overrides
        public override bool Enabled => 
            THK != 0 && 
            Header.PartitionBelowRow2 != 0 && 
            Header.PartitionDistanceBelow2 != 0;
        public override List<PositionData> Position
        {
            get
            {
                if (_pos == null)
                {
                    _pos = new List<PositionData>
                    {
                        Position1,
                        Position2
                    };
                }
                return _pos;
            }
        }


        // Private properties
        static private double LocationBelowRowNumber2
        {
            get
            {
                int headerNo = int.Parse(Header.GetType().Name.Split('_')[1]);
                switch (headerNo)
                {
                    case 61:
                        return Header61.PartitionBelowRow2;
                    case 62:
                        return Header62.PartitionBelowRow2;
                    case 63:
                        return Header63.PartitionBelowRow2 - Header61.NumberOfRows;
                    case 64:
                        return Header64.PartitionBelowRow2 - Header62.NumberOfRows;
                    case 65:
                        return Header65.PartitionBelowRow2 - Header63.NumberOfRows - Header61.NumberOfRows;
                    case 66:
                        return Header66.PartitionBelowRow2 - Header64.NumberOfRows - Header62.NumberOfRows;
                    default:
                        throw new Exception($"{headerNo} is an invalid header number");
                }
            }
        }
    }
}
