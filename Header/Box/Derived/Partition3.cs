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
    internal class Partition3 : Partition
    {
        // Static properties
        static public bool IsRequired =>
            THK != 0 &&
            Header.PartitionBelowRow3 != 0 &&
            Header.PartitionDistanceBelow3 != 0;


        // Constructor
        public Partition3(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Public properties
        public static PositionData Position3 => PositionData.Create(tY: -GetYTranslation(
            Header.PartitionDistanceBelow3,
            LocationBelowRowNumber3));


        // Private properties
        static private double LocationBelowRowNumber3
        {
            get
            {
                int headerNo = int.Parse(Header.GetType().Name.Split('_')[1]);
                switch (headerNo)
                {
                    case 61:return Header61.PartitionBelowRow3;
                    case 62:return Header62.PartitionBelowRow3;
                    case 63:return Header63.PartitionBelowRow3 - Header61.NumberOfRows;
                    case 64:return Header64.PartitionBelowRow3 - Header62.NumberOfRows;
                    case 65:return Header65.PartitionBelowRow3 - Header63.NumberOfRows - Header61.NumberOfRows;
                    case 66:return Header66.PartitionBelowRow3 - Header64.NumberOfRows - Header62.NumberOfRows;
                    default:
                        throw new Exception($"{headerNo} is an invalid header number");
                }
            }
        }
    }
}
