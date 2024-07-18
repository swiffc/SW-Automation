using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static HDR.HeaderBase;
using static FileTools.CommonData.CommonData;


namespace HDR.Box.Derived
{
    internal class Stiffener2 : Stiffener
    {
        // Static properties
        static public bool IsRequired =>
            THK != 0 &&
            Header.StiffenerBelowRow2 != 0 &&
            Header.StiffenerDistanceBelow2 != 0;


        // Constructor
        public Stiffener2(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Protected properties
        public static PositionData Position2 => PositionData.Create(tY: -GetYTranslation(
            Header.StiffenerDistanceBelow2,
            LocationBelowRowNumber2));


        // Private properties
        static private double LocationBelowRowNumber2
        {
            get
            {
                int headerNo = int.Parse(Header.GetType().Name.Split('_')[1]);
                switch (headerNo)
                {
                    case 61:
                        return Header61.StiffenerBelowRow2;
                    case 62:
                        return Header62.StiffenerBelowRow2;
                    case 63:
                        return Header63.StiffenerBelowRow2 - Header61.NumberOfRows;
                    case 64:
                        return Header64.StiffenerBelowRow2 - Header62.NumberOfRows;
                    case 65:
                        return Header65.StiffenerBelowRow2 - Header63.NumberOfRows - Header61.NumberOfRows;
                    case 66:
                        return Header66.StiffenerBelowRow2 - Header64.NumberOfRows - Header62.NumberOfRows;
                    default:
                        throw new Exception($"{headerNo} is an invalid header number");
                }
            }
        }
    }
}
