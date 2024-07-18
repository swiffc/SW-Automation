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


        // Constructor
        public Partition(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Protected overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Plate", Length);
            EditDimension("Width", "sk:Plate", Width);
            EditDimension("THK", "Plate", THK);
        }


        // Protected methods
        protected double GetYTranslation(double distanceBelow, double locationBelowRowNumber)
        {
            double value = distanceBelow;
            for (int i = 0; i < locationBelowRowNumber; i++)
            {
                value += TubeSheet.HoleLocations[i];
            }
            return value;
        }


        // Protected properties
        protected PositionData Position1 => PositionData.Create(tY: -GetYTranslation(
            Header.PartitionDistanceBelow, 
            LocationBelowRowNumber));


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
                    _pos =  new List<PositionData>
                    {
                        Position1
                    };
                }
                return _pos;
            }
        }


        // Wrapper properties
        static protected double THK => Header.PartitionTHK;
        static protected double Width => Header.PartitionWidth > Header.BoxWidth ? Header.BoxWidth : Header.PartitionWidth;
        static private double LocationBelowRowNumber
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
