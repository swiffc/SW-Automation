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
    internal class Stiffener3 : Stiffener
    {
        // Static properties
        static public bool IsRequired =>
            THK != 0 &&
            Header.StiffenerBelowRow3 != 0 &&
            Header.StiffenerDistanceBelow3 != 0;


        // Constructor
        public Stiffener3(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Protected properties
        public static PositionData Position3 => PositionData.Create(tY: -GetYTranslation(
            Header.StiffenerDistanceBelow3,
            LocationBelowRowNumber3));


        // Private properties
        static private double LocationBelowRowNumber3
        {
            get
            {
                int headerNo = int.Parse(Header.GetType().Name.Split('_')[1]);
                switch (headerNo)
                {
                    case 61: return Header61.StiffenerBelowRow3;
                    case 62: return Header62.StiffenerBelowRow3;
                    case 63: return Header63.StiffenerBelowRow3 - Header61.NumberOfRows;
                    case 64: return Header64.StiffenerBelowRow3 - Header62.NumberOfRows;
                    case 65: return Header65.StiffenerBelowRow3 - Header63.NumberOfRows - Header61.NumberOfRows;
                    case 66: return Header66.StiffenerBelowRow3 - Header64.NumberOfRows - Header62.NumberOfRows;
                    default:
                        throw new Exception($"{headerNo} is an invalid header number");
                }
            }
        }
    }
}
