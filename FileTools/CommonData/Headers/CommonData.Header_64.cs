using FileTools.Base;
using static FileTools.Properties.Settings;

namespace FileTools.CommonData
{
    public static partial class CommonData
    {// 64 Header
        public static Header_64 Header64 = new Header_64();
        public class Header_64 : IHeaderExtensions
        {
            public double BoxWidth
            {
                get => Default.BoxWidth64;
                set => Default.BoxWidth64 = value;
            }
            public double TubesheetTHK
            {
                get => Default.TubesheetTHK_64;
                set => Default.TubesheetTHK_64 = value;
            }
            public double PlugsheetTHK
            {
                get => Default.PlugsheetTHK_64;
                set => Default.PlugsheetTHK_64 = value;
            }
            public bool IsRequired
            {
                get => Default.IsRequired_64;
                set => Default.IsRequired_64 = value;
            }
            public double BoxHeight
            {
                get => Default.VerticalSpan64;
                set => Default.VerticalSpan64 = value;
            }
            public double BoxLength
            {
                get => Default.BoxLength64;
                set => Default.BoxLength64 = value;
            }
            public double TopAndBottomPlateTHK
            {
                get => Default.TopBottomTHK_64;
                set => Default.TopBottomTHK_64 = value;
            }
            public double TubeY
            {
                get => Default.Y_Location64;
                set => Default.Y_Location64 = value;
            }
            public double TubesheetLength
            {
                get => Default.TubesheetLength_64;
                set => Default.TubesheetLength_64 = value;
            }
            public double TubesheetWidth
            {
                get => Default.TubesheetWidth_64;
                set => Default.TubesheetWidth_64 = value;
            }
            public double EndPlateTHK
            {
                get => Default.EndPlateTHK_64;
                set => Default.EndPlateTHK_64 = value;
            }
            public double TopBtmTHK
            {
                get => Default.TopBtmTHK_64;
                set => Default.TopBtmTHK_64 = value;
            }
            public double PlugsheetWidth
            {
                get => Default.PlugSheetWidth_64;
                set => Default.PlugSheetWidth_64 = value;
            }
            public double PlugsheetLength
            {
                get => Default.PlugSheetLength_64;
                set => Default.PlugSheetLength_64 = value;
            }
            public double EndPlateWidth
            {
                get => Default.EndPlateWidth_64;
                set => Default.EndPlateWidth_64 = value;
            }
            public double EndPlateLength
            {
                get => Default.EndPlateLength_64;
                set => Default.EndPlateLength_64 = value;
            }
            public double TopBtmWidth
            {
                get => Default.TopBtmWidth_64;
                set => Default.TopBtmWidth_64= value;
            }
            public double TopBtmLength
            {
                get => Default.TopBtmLength_64;
                set => Default.TopBtmLength_64 = value;
            }
            public double TubeHoleDiameter
            {
                get => Default.TubeHoleDiameter_64;
                set => Default.TubeHoleDiameter_64 = value;
            }
            public double TubeEvenX
            {
                get => Default.TubeEvenX_64;
                set => Default.TubeEvenX_64 = value;
            }
            public double TubeOddX
            {
                get => Default.TubeOddX_64;
                set => Default.TubeOddX_64 = value;
            }
            public double TubeRow1Count
            {
                get => Default.TubeRow1Count_64;
                set => Default.TubeRow1Count_64 = value;
            }
            public double TubeRow2Count
            {
                get => Default.TubeRow2Count_64;
                set => Default.TubeRow2Count_64 = value;
            }
            public double TubeRow3Count
            {
                get => Default.TubeRow3Count_64;
                set => Default.TubeRow3Count_64 = value;
            }
            public double TubeRow4Count
            {
                get => Default.TubeRow4Count_64;
                set => Default.TubeRow4Count_64 = value;
            }
            public double TubeRow5Count
            {
                get => Default.TubeRow5Count_64;
                set => Default.TubeRow5Count_64 = value;
            }
            public double TubeRow6Count
            {
                get => Default.TubeRow6Count_64;
                set => Default.TubeRow6Count_64 = value;
            }
            public double TubeRow7Count
            {
                get => Default.TubeRow7Count_64;
                set => Default.TubeRow7Count_64 = value;
            }
            public double TubeRow8Count
            {
                get => Default.TubeRow8Count_64;
                set => Default.TubeRow8Count_64 = value;
            }
            public double TubeRow9Count
            {
                get => Default.TubeRow9Count_64;
                set => Default.TubeRow9Count_64 = value;
            }
            public double TubeRow10Count
            {
                get => Default.TubeRow10Count_64;
                set => Default.TubeRow10Count_64 = value;
            }
            public double TubeHPitchOdd
            {
                get => Default.TubeHPitchOdd_64;
                set => Default.TubeHPitchOdd_64 = value;
            }
            public double TubeHPitchEven
            {
                get => Default.TubeHPitchEven_64;
                set => Default.TubeHPitchEven_64 = value;
            }
            public double TubeVPitchOneTwo
            {
                get => Default.TubeVPitchOneTwo_64;
                set => Default.TubeVPitchOneTwo_64 = value;
            }
            public double TubeVPitchTwoThree
            {
                get => Default.TubeVPitchTwoThree_64;
                set => Default.TubeVPitchTwoThree_64 = value;
            }
            public double TubeVPitchThreeFour
            {
                get => Default.TubeVPitchThreeFour_64;
                set => Default.TubeVPitchThreeFour_64 = value;
            }
            public double TubeVPitchFourFive
            {
                get => Default.TubeVPitchFourFive_64;
                set => Default.TubeVPitchFourFive_64 = value;
            }
            public double TubeVPitchFiveSix
            {
                get => Default.TubeVPitchFiveSix_64;
                set => Default.TubeVPitchFiveSix_64 = value;
            }
            public double TubeVPitchSixSeven
            {
                get => Default.TubeVPitchSixSeven_64;
                set => Default.TubeVPitchSixSeven_64 = value;
            }
            public double TubeVPitchSevenEight
            {
                get => Default.TubeVPitchSevenEight_64;
                set => Default.TubeVPitchSevenEight_64 = value;
            }
            public double TubeVPitchEightNine
            {
                get => Default.TubeVPitchEightNine_64;
                set => Default.TubeVPitchEightNine_64 = value;
            }
            public double TubeVPitchNineTen
            {
                get => Default.TubeVPitchNineTen_64;
                set => Default.TubeVPitchNineTen_64 = value;
            }
            public double TubeVPitchTenEleven
            {
                get => Default.TubeVPitchTenEleven_64;
                set => Default.TubeVPitchTenEleven_64 = value;
            }
            public double TubeVPitchElevenTwelve
            {
                get => Default.TubeVPitchElevenTwelve_64;
                set => Default.TubeVPitchElevenTwelve_64 = value;
            }
            public bool IsBusted
            {
                get => Default.IsBusted_64;
                set => Default.IsBusted_64 = value;
            }
        }
    }
}
