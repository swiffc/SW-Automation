using FileTools.Base;
using static FileTools.Properties.Settings;

namespace FileTools.CommonData
{
    public static partial class CommonData
    {// 62 Header
        public static Header_62 Header62 = new Header_62();
        public class Header_62 : IHeaderExtensions
        {
            public double BoxWidth
            {
                get => Default.BoxWidth62;
                set => Default.BoxWidth62 = value;
            }
            public double TubesheetTHK
            {
                get => Default.TubesheetTHK_62;
                set => Default.TubesheetTHK_62 = value;
            }
            public double PlugsheetTHK
            {
                get => Default.PlugsheetTHK_62;
                set => Default.PlugsheetTHK_62 = value;
            }
            public bool IsRequired
            {
                get => Default.IsRequired_62;
                set => Default.IsRequired_62 = value;
            }
            public double BoxHeight
            {
                get => Default.VerticalSpan62;
                set => Default.VerticalSpan62 = value;
            }
            public double BoxLength
            {
                get => Default.BoxLength62;
                set => Default.BoxLength62 = value;
            }
            public double TopAndBottomPlateTHK
            {
                get => Default.TopBottomTHK_62;
                set => Default.TopBottomTHK_62 = value;
            }
            public double TubeY
            {
                get => Default.Y_Location62;
                set => Default.Y_Location62 = value;
            }
            public double TubeOddX
            {
                get => Default.Xtop_62;
                set => Default.Xtop_62 = value;
            }
            public double TubesheetLength
            {
                get => Default.TubesheetLength_62;
                set => Default.TubesheetLength_62 = value;
            }
            public double TubesheetWidth
            {
                get => Default.TubesheetWidth_62;
                set => Default.TubesheetWidth_62 = value;
            }
            public double EndPlateTHK
            {
                get => Default.EndPlateTHK_62;
                set => Default.EndPlateTHK_62 = value;
            }
            public double TopBtmTHK
            {
                get => Default.TopBtmTHK_62;
                set => Default.TopBtmTHK_62 = value;
            }
            public double PlugsheetWidth
            {
                get => Default.PlugSheetWidth_62;
                set => Default.PlugSheetWidth_62 = value;
            }
            public double PlugsheetLength
            {
                get => Default.PlugSheetLength_62;
                set => Default.PlugSheetLength_62 = value;
            }
            public double EndPlateWidth
            {
                get => Default.EndPlateWidth_62;
                set => Default.EndPlateWidth_62 = value;
            }
            public double EndPlateLength
            {
                get => Default.EndPlateLength_62;
                set => Default.EndPlateLength_62 = value;
            }
            public double TopBtmWidth
            {
                get => Default.TopBtmWidth_62;
                set => Default.TopBtmWidth_62 = value;
            }
            public double TopBtmLength
            {
                get => Default.TopBtmLength_62;
                set => Default.TopBtmLength_62 = value;
            }
            public double TubeHoleDiameter
            {
                get => Default.TubeHoleDiameter_62;
                set => Default.TubeHoleDiameter_62 = value;
            }
            public double TubeEvenX
            {
                get => Default.TubeEvenX_62;
                set => Default.TubeEvenX_62 = value;
            }
            public double TubeRow1Count
            {
                get => Default.TubeRow1Count_62;
                set => Default.TubeRow1Count_62 = value;
            }
            public double TubeRow2Count
            {
                get => Default.TubeRow2Count_62;
                set => Default.TubeRow2Count_62 = value;
            }
            public double TubeRow3Count
            {
                get => Default.TubeRow3Count_62;
                set => Default.TubeRow3Count_62 = value;
            }
            public double TubeRow4Count
            {
                get => Default.TubeRow4Count_62;
                set => Default.TubeRow4Count_62 = value;
            }
            public double TubeRow5Count
            {
                get => Default.TubeRow5Count_62;
                set => Default.TubeRow5Count_62 = value;
            }
            public double TubeRow6Count
            {
                get => Default.TubeRow6Count_62;
                set => Default.TubeRow6Count_62 = value;
            }
            public double TubeRow7Count
            {
                get => Default.TubeRow7Count_62;
                set => Default.TubeRow7Count_62 = value;
            }
            public double TubeRow8Count
            {
                get => Default.TubeRow8Count_62;
                set => Default.TubeRow8Count_62 = value;
            }
            public double TubeRow9Count
            {
                get => Default.TubeRow9Count_62;
                set => Default.TubeRow9Count_62 = value;
            }
            public double TubeRow10Count
            {
                get => Default.TubeRow10Count_62;
                set => Default.TubeRow10Count_62 = value;
            }
            public double TubeRow11Count
            {
                get => Default.TubeRow11Count_62;
                set => Default.TubeRow11Count_62 = value;
            }
            public double TubeRow12Count
            {
                get => Default.TubeRow12Count_62;
                set => Default.TubeRow12Count_62 = value;
            }
            public double TubeHPitchOdd
            {
                get => Default.TubeHPitchOdd_62;
                set => Default.TubeHPitchOdd_62 = value;
            }
            public double TubeHPitchEven
            {
                get => Default.TubeHPitchEven_62;
                set => Default.TubeHPitchEven_62 = value;
            }
            public double TubeVPitchOneTwo
            {
                get => Default.TubeVPitchOneTwo_62;
                set => Default.TubeVPitchOneTwo_62 = value;
            }
            public double TubeVPitchTwoThree
            {
                get => Default.TubeVPitchTwoThree_62;
                set => Default.TubeVPitchTwoThree_62 = value;
            }
            public double TubeVPitchThreeFour
            {
                get => Default.TubeVPitchThreeFour_62;
                set => Default.TubeVPitchThreeFour_62 = value;
            }
            public double TubeVPitchFourFive
            {
                get => Default.TubeVPitchFourFive_62;
                set => Default.TubeVPitchFourFive_62 = value;
            }
            public double TubeVPitchFiveSix
            {
                get => Default.TubeVPitchFiveSix_62;
                set => Default.TubeVPitchFiveSix_62 = value;
            }
            public double TubeVPitchSixSeven
            {
                get => Default.TubeVPitchSixSeven_62;
                set => Default.TubeVPitchSixSeven_62 = value;
            }
            public double TubeVPitchSevenEight
            {
                get => Default.TubeVPitchSevenEight_62;
                set => Default.TubeVPitchSevenEight_62 = value;
            }
            public double TubeVPitchEightNine
            {
                get => Default.TubeVPitchEightNine_62;
                set => Default.TubeVPitchEightNine_62 = value;
            }
            public double TubeVPitchNineTen
            {
                get => Default.TubeVPitchNineTen_62;
                set => Default.TubeVPitchNineTen_62 = value;
            }
            public double TubeVPitchTenEleven
            {
                get => Default.TubeVPitchTenEleven_62;
                set => Default.TubeVPitchTenEleven_62 = value;
            }
            public double TubeVPitchElevenTwelve
            {
                get => Default.TubeVPitchElevenTwelve_62;
                set => Default.TubeVPitchElevenTwelve_62 = value;
            }
            public bool IsBusted
            {
                get => Default.IsBusted_62;
                set => Default.IsBusted_62 = value;
            }
            public double StiffenerTHK
            {
                get => Default.StiffenerTHK_62;
                set => Default.StiffenerTHK_62 = value;
            }
            public double StiffenerWidth
            {
                get => Default.StiffenerWidth_62;
                set => Default.StiffenerWidth_62 = value;
            }
            public double StiffenerOffset
            {
                get => Default.StiffenerLength_62;
                set => Default.StiffenerLength_62 = value;
            }
            public double StiffenerBelowRow
            {
                get => Default.StiffenerBelowRow_62;
                set => Default.StiffenerBelowRow_62 = value;
            }
            public double StiffenerDistanceBelow
            {
                get => Default.StiffenerDistanceBelow_62;
                set => Default.StiffenerDistanceBelow_62 = value;
            }
            public int NumberOfRows
            {
                get
                {
                    return HeaderHelper.RowCounter(Header62);
                }
            }

            public double StiffenerWindowWidth
            {
                get => Default.StiffenerWindowWidth_62;
                set => Default.StiffenerWindowWidth_62 = value;
            }
            public double StiffenerWindowLength
            {
                get => Default.StiffenerWindowLength_62;
                set => Default.StiffenerWindowLength_62 = value;
            }
            public double StiffenerWindowQuantity
            {
                get => Default.StiffenerWindowQuantity_62;
                set => Default.StiffenerWindowQuantity_62 = value;
            }
            public double StiffenerWindowSpacing
            {
                get => Default.StiffenerWindowSpacing_62;
                set => Default.StiffenerWindowSpacing_62 = value;
            }
            public double StiffenerWindowOffset
            {
                get => Default.StiffenerWindowOffset_62;
                set => Default.StiffenerWindowOffset_62 = value;
            }
        }
    }
}
