using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static HDR.HeaderBase;
using static FileTools.CommonData.CommonData;
using HDR.Box.Derived;

namespace HDR.Box
{
    internal class Stiffener : Part
    {
        // Static properties
        static public double Length => TubeSheet.Length - EndPlate.THK * 2 - Header.StiffenerOffset * 2;


        // Constructor
        public Stiffener(SW_Assembly parentMainAssembly) : base(parentMainAssembly) 
        {
            var loadPos = Position;
        }


        // Protected overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Plate", Length);
            EditDimension("Width", "sk:Plate", Width);
            EditDimension("THK", "Plate", THK);

            EditDimension("Width", "sk:Window", Header.StiffenerWindowWidth);
            EditDimension("Length", "sk:Window", Header.StiffenerWindowLength);
            EditDimension("Count", "sk:Window", Header.StiffenerWindowQuantity);
            EditDimension("Spacing", "sk:Window", Header.StiffenerWindowSpacing);
            EditDimension("Offset", "sk:Window", Header.StiffenerWindowOffset);
        }


        // Public methods
        public static void ClearPositionData()
        {
            _staticPos = null;
            _pos1 = null;
            _pos2 = null;
            _pos3 = null;
        }


        // Protected methods
        protected static double GetYTranslation(double distanceBelow, double locationBelowRowNumber)
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
            Header.StiffenerDistanceBelow,
            LocationBelowRowNumber));


        // Property overrides
        public override string PartNo => Header.StiffenerPartNo;
        public override bool Enabled => THK != 0;
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
        public override string StaticPartNo => "Stiffener";
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
                if (_pos2 == null && Stiffener2.IsRequired)
                {
                    _pos2 = new List<PositionData>
                    {
                        Stiffener2.Position2
                    };
                    _staticPos.AddRange(_pos2);
                }

                // 3
                if (_pos3 == null && Stiffener3.IsRequired)
                {
                    _pos3 = new List<PositionData>
                    {
                        Stiffener3.Position3
                    };
                    _staticPos.AddRange(_pos3);
                }

                return _staticPos;
            }
        }


        // Wrapper properties
        static public double THK => Header.StiffenerTHK;
        static public double Width => Header.StiffenerWidth > Header.BoxWidth ? Header.BoxWidth : Header.StiffenerWidth;
        static public double LocationBelowRowNumber
        {
            get
            {
                int headerNo = int.Parse(Header.GetType().Name.Split('_')[1]);
                switch (headerNo)
                {
                    case 61:
                        return Header61.StiffenerBelowRow;
                    case 62:
                        return Header62.StiffenerBelowRow;
                    case 63:
                        return Header63.StiffenerBelowRow - Header61.NumberOfRows;
                    case 64:
                        return Header64.StiffenerBelowRow - Header62.NumberOfRows;
                    case 65:
                        return Header65.StiffenerBelowRow - Header63.NumberOfRows - Header61.NumberOfRows;
                    case 66:
                        return Header66.StiffenerBelowRow - Header64.NumberOfRows - Header62.NumberOfRows;
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
