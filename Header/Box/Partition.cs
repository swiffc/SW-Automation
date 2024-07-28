using FileTools.Base;
using HDR.Box.Derived;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using static HDR.HeaderBase;
using static FileTools.Base.SW_Assembly;

namespace HDR.Box
{
    internal class Partition : Part
    {
        // Static properties
        static public double Length => TubeSheet.Length - EndPlate.THK * 2;


        // Constructor
        public Partition(SW_Assembly parentMainAssembly) : base(parentMainAssembly)
        {
            var loadPos = Position;

            DontProcessLocation.Add(typeof(Partition2));
            DontProcessLocation.Add(typeof(Partition3));
        }


        // Protected overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Plate", Length);
            EditDimension("Width", "sk:Plate", Width);
            EditDimension("THK", "Plate", THK);
        }


        // Internal methods
        internal static double GetYTranslation(double distanceBelow, double locationBelowRowNumber)
        {
            List<double> holeLocations = new List<double>
            {
                Header.TubeY,
                Header.TubeVPitchOneTwo,
                Header.TubeVPitchTwoThree,
                Header.TubeVPitchThreeFour,
                Header.TubeVPitchFourFive,
                Header.TubeVPitchFiveSix,
                Header.TubeVPitchSixSeven,
                Header.TubeVPitchSevenEight,
                Header.TubeVPitchEightNine,
                Header.TubeVPitchNineTen,
                Header.TubeVPitchTenEleven,
                Header.TubeVPitchElevenTwelve
            };

            double value = distanceBelow;
            for (int i = 0; i < locationBelowRowNumber; i++)
            {
                value += holeLocations[i];
            }
            return value;
        }


        // Public methods
        public static void ClearPositionData()
        {
            _staticPos = null;
            _pos1 = null;
            _pos2 = null;
            _pos3 = null;
        }


        // Protected properties
        protected PositionData Position1 => PositionData.Create(tY: -GetYTranslation(Header.PartitionDistanceBelow, LocationBelowRowNumber));


        // Property overrides
        public override string PartNo
        {
            get
            {
                if (Header.PartitionPartNo != null)
                {
                    if (Header.PartitionPartNo.Length != 0)
                        return Header.PartitionPartNo;
                }
                return "Partition";
            }
        }
        public override bool Enabled => THK != 0;
        public override string StaticPartNo => "Partition";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                if (_staticPos == null) { _staticPos = new List<PositionData>(); }

                // 1
                if (_pos1 == null)
                {
                    _pos1 = new List<PositionData>
                    {
                        Position1
                    };
                    _staticPos.AddRange(_pos1);
                }

                // 2
                if (_pos2 == null && Partition2.IsRequired)
                {
                    _pos2 = new List<PositionData>
                    {
                        Partition2.Position2
                    };
                    _staticPos.AddRange(_pos2);
                }

                // 3
                if (_pos3 == null && Partition3.IsRequired)
                {
                    _pos3 = new List<PositionData>
                    {
                        Partition3.Position3
                    };
                    _staticPos.AddRange(_pos3);
                }

                return _staticPos;
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


        // Backing fields
        static List<PositionData> _staticPos;
        static List<PositionData> _pos1;
        static List<PositionData> _pos2;
        static List<PositionData> _pos3;
    }
}
