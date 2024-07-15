using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.Properties.Settings;

namespace FileTools.Base
{
    public interface IHeaderExtensions
    {
        bool IsRequired { get; set; }

        double BoxWidth { get; set; }
        double BoxHeight { get; set; }
        double BoxLength { get; set; }

        double TubesheetTHK { get; set; }
        double TubesheetLength { get; set; }
        double TubesheetWidth { get; set; }
        double PlugsheetTHK { get; set; }
        double TopAndBottomPlateTHK { get; set; }

        double TubeY { get; set; }

        double EndPlateTHK { get; set; }
        double TopBtmTHK { get; set; }

        double PlugsheetLength { get; set; }
        double PlugsheetWidth { get; set; }

        double EndPlateLength { get; set; }
        double EndPlateWidth { get; set; }
        double TopBtmLength { get; set; }
        double TopBtmWidth { get; set; }

        double TubeHoleDiameter { get; set; }
        double TubeOddX { get; set; }
        double TubeEvenX { get; set; }

        double TubeRow1Count { get; set; }
        double TubeRow2Count { get; set; }
        double TubeRow3Count { get; set; }
        double TubeRow4Count { get; set; }
        double TubeRow5Count { get; set; }
        double TubeRow6Count { get; set; }
        double TubeRow7Count { get; set; }
        double TubeRow8Count { get; set; }
        double TubeRow9Count { get; set; }
        double TubeRow10Count { get; set; }
        double TubeRow11Count { get; set; }
        double TubeRow12Count { get; set; }
        int NumberOfRows { get; }

        double TubeHPitchOdd { get; set; }
        double TubeHPitchEven { get; set; }

        double TubeVPitchOneTwo { get; set; }
        double TubeVPitchTwoThree { get; set; }
        double TubeVPitchThreeFour { get; set; }
        double TubeVPitchFourFive { get; set; }
        double TubeVPitchFiveSix { get; set; }
        double TubeVPitchSixSeven { get; set; }
        double TubeVPitchSevenEight { get; set; }
        double TubeVPitchEightNine { get; set; }
        double TubeVPitchNineTen { get; set; }
        double TubeVPitchTenEleven { get; set; }
        double TubeVPitchElevenTwelve { get; set; }

        bool IsBusted { get; set; }

        double StiffenerTHK { get; set; }
        double StiffenerWidth { get; set; }
        double StiffenerOffset { get; set; }
        double StiffenerBelowRow { get; set; }
        double StiffenerDistanceBelow { get; set; }
        double StiffenerWindowWidth { get; set; }
        double StiffenerWindowLength { get; set; }
        double StiffenerWindowQuantity { get; set; } 
        double StiffenerWindowSpacing { get; set; }
        double StiffenerWindowOffset { get; set; }
        double PartitionTHK { get; set; }
        double PartitionWidth { get; set; }
        double PartitionBelowRow { get; set; }
        double PartitionDistanceBelow { get; set; }

        double FootHeight { get; set; }

        double EndPlateBustedSpan2 { get; set; }
    }


    public static class HeaderHelper
    {
        public static int RowCounter(IHeaderExtensions header)
        {
            int count = 0;
            if (header.TubeRow1Count > 0) count++;
            if (header.TubeRow2Count > 0) count++;
            if (header.TubeRow3Count > 0) count++;
            if (header.TubeRow4Count > 0) count++;
            if (header.TubeRow5Count > 0) count++;
            if (header.TubeRow6Count > 0) count++;
            if (header.TubeRow7Count > 0) count++;
            if (header.TubeRow8Count > 0) count++;
            if (header.TubeRow9Count > 0) count++;
            if (header.TubeRow10Count > 0) count++;
            if (header.TubeRow11Count > 0) count++;
            if (header.TubeRow12Count > 0) count++;
            return count;
        }
    }
  
}
