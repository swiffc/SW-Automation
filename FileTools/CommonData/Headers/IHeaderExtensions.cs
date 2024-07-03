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
    }
}
