using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using static HDR.HeaderBase;

namespace HDR.Box
{
    internal class Partition : Part
    {
        // Static properties
        static public double Length => TubeSheet.Length - EndPlate.THK * 2;


        // Private properties
        private double YTranslation
        {
            get
            {
                double value = Header.PartitionDistanceBelow;
                for (int i = 0; i < LocationBelowRowNumber; i++)
                {
                    value += TubeSheet.HoleLocations[i];
                }
                return value;
            }
        }


        // Constructor
        public Partition(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Protected overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Plate", Length);
            EditDimension("Width", "sk:Plate", Width);
            EditDimension("THK", "Plate", THK);
        }


        // Property overrides
        public override string PartNo => Header.PartitionPartNo;
        public override bool Enabled => THK != 0;
        public override string StaticPartNo => "Partition";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                if (_pos == null)
                {
                    double xTranslation = 0;
                    double yTranslation = -YTranslation;
                    double zTranslation = 0;

                    return new List<PositionData>
                    {
                        PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation),
                    };
                }
                return _pos;
            }
        }


        // Wrapper properties
        static public double THK => Header.PartitionTHK;
        static public double Width => Header.PartitionWidth > Header.BoxWidth ? Header.BoxWidth : Header.PartitionWidth;
        static public double LocationBelowRowNumber
        {
            get
            {
                int headerNo = int.Parse(Header.GetType().Name.Split('_')[1]);
                switch (headerNo)
                {
                    case 61:
                        return Header61.PartitionBelowRow;
                    case 62:
                        return Header62.PartitionBelowRow;
                    case 63:
                        return Header63.PartitionBelowRow - Header61.NumberOfRows;
                    case 64:
                        return Header64.PartitionBelowRow - Header62.NumberOfRows;
                    case 65:
                        return Header65.PartitionBelowRow - Header63.NumberOfRows - Header61.NumberOfRows;
                    case 66:
                        return Header66.PartitionBelowRow - Header64.NumberOfRows - Header62.NumberOfRows;
                    default:
                        throw new Exception($"{headerNo} is an invalid header number");
                }
            }
        }
    }
}
