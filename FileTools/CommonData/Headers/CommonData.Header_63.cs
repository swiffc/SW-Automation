using FileTools.Base;
using static FileTools.Properties.Settings;

namespace FileTools.CommonData
{
    public static partial class CommonData
    {// 63 Header
        public static Header_63 Header63 = new Header_63();
        public class Header_63 : IHeaderExtensions
        {
            public double BoxWidth
            {
                get => Default.BoxWidth63;
                set => Default.BoxWidth63 = value;
            }
            public double TubesheetTHK
            {
                get => Default.TubesheetTHK_63;
                set => Default.TubesheetTHK_63 = value;
            }
            public double PlugsheetTHK
            {
                get => Default.PlugsheetTHK_63;
                set => Default.PlugsheetTHK_63 = value;
            }
            public bool IsRequired
            {
                get => Default.IsRequired_63;
                set => Default.IsRequired_63 = value;
            }
            public double BoxHeight
            {
                get => Default.VerticalSpan63;
                set => Default.VerticalSpan63 = value;
            }
            public double BoxLength
            {
                get => Default.BoxLength63;
                set => Default.BoxLength63 = value;
            }
            public double TopAndBottomPlateTHK
            {
                get => Default.TopBottomTHK_63;
                set => Default.TopBottomTHK_63 = value;
            }
            public double TubeY
            {
                get => Default.Y_Location63;
                set => Default.Y_Location63 = value;
            }
            public double TubesheetLength
            {
                get => Default.TubesheetLength_63;
                set => Default.TubesheetLength_63 = value;
            }
            public double TubesheetWidth
            {
                get => Default.TubesheetWidth_63;
                set => Default.TubesheetWidth_63 = value;
            }
            public double EndPlateTHK
            {
                get => Default.EndPlateTHK_63;
                set => Default.EndPlateTHK_63 = value;
            }
            public double TopBtmTHK
            {
                get => Default.TopBtmTHK_63;
                set => Default.TopBtmTHK_63 = value;
            }
            public double PlugsheetWidth
            {
                get => Default.PlugSheetWidth_63;
                set => Default.PlugSheetWidth_63 = value;
            }
            public double PlugsheetLength
            {
                get => Default.PlugSheetLength_63;
                set => Default.PlugSheetLength_63 = value;
            }
            public double EndPlateWidth
            {
                get => Default.EndPlateWidth_63;
                set => Default.EndPlateWidth_63 = value;
            }
            public double EndPlateLength
            {
                get => Default.EndPlateLength_63;
                set => Default.EndPlateLength_63 = value;
            }
            public double TopBtmWidth
            {
                get => Default.TopBtmWidth_63;
                set => Default.TopBtmWidth_63 = value;
            }
            public double TopBtmLength
            {
                get => Default.TopBtmLength_63;
                set => Default.TopBtmLength_63 = value;
            }
            public double TubeHoleDiameter
            {
                get => Default.TubeHoleDiameter_63;
                set => Default.TubeHoleDiameter_63 = value;
            }
            public double TubeEvenX
            {
                get => Default.TubeEvenX_63;
                set => Default.TubeEvenX_63 = value;
            }
            public double TubeOddX 
            {
                get => Default.TubeOddX_63; 
                set => Default.TubeOddX_63 = value; 
            }
            public double TubeRow1Count
            {
                get => Default.TubeRow1Count_63;
                set => Default.TubeRow1Count_63 = value;
            }
            public double TubeRow2Count
            {
                get => Default.TubeRow2Count_63;
                set => Default.TubeRow2Count_63 = value;
            }
            public double TubeRow3Count
            {
                get => Default.TubeRow3Count_63;
                set => Default.TubeRow3Count_63 = value;
            }
            public double TubeRow4Count
            {
                get => Default.TubeRow4Count_63;
                set => Default.TubeRow4Count_63 = value;
            }
            public double TubeRow5Count
            {
                get => Default.TubeRow5Count_63;
                set => Default.TubeRow5Count_63 = value;
            }
            public double TubeRow6Count
            {
                get => Default.TubeRow6Count_63;
                set => Default.TubeRow6Count_63 = value;
            }
            public double TubeRow7Count
            {
                get => Default.TubeRow7Count_63;
                set => Default.TubeRow7Count_63 = value;
            }
            public double TubeRow8Count
            {
                get => Default.TubeRow8Count_63;
                set => Default.TubeRow8Count_63 = value;
            }
            public double TubeRow9Count
            {
                get => Default.TubeRow9Count_63;
                set => Default.TubeRow9Count_63 = value;
            }
            public double TubeRow10Count
            {
                get => Default.TubeRow10Count_63;
                set => Default.TubeRow10Count_63 = value;
            }
            public double TubeRow11Count
            {
                get => Default.TubeRow11Count_63;
                set => Default.TubeRow11Count_63 = value;
            }
            public double TubeRow12Count
            {
                get => Default.TubeRow12Count_63;
                set => Default.TubeRow12Count_63 = value;
            }
            public double TubeHPitchOdd
            {
                get => Default.TubeHPitchOdd_63;
                set => Default.TubeHPitchOdd_63 = value;
            }
            public double TubeHPitchEven
            {
                get => Default.TubeHPitchEven_63;
                set => Default.TubeHPitchEven_63 = value;
            }
            public double TubeVPitchOneTwo
            {
                get => Default.TubeVPitchOneTwo_63;
                set => Default.TubeVPitchOneTwo_63 = value;
            }
            public double TubeVPitchTwoThree
            {
                get => Default.TubeVPitchTwoThree_63;
                set => Default.TubeVPitchTwoThree_63 = value;
            }
            public double TubeVPitchThreeFour
            {
                get => Default.TubeVPitchThreeFour_63;
                set => Default.TubeVPitchThreeFour_63 = value;
            }
            public double TubeVPitchFourFive
            {
                get => Default.TubeVPitchFourFive_63;
                set => Default.TubeVPitchFourFive_63 = value;
            }
            public double TubeVPitchFiveSix
            {
                get => Default.TubeVPitchFiveSix_63;
                set => Default.TubeVPitchFiveSix_63 = value;
            }
            public double TubeVPitchSixSeven
            {
                get => Default.TubeVPitchSixSeven_63;
                set => Default.TubeVPitchSixSeven_63 = value;
            }
            public double TubeVPitchSevenEight
            {
                get => Default.TubeVPitchSevenEight_63;
                set => Default.TubeVPitchSevenEight_63 = value;
            }
            public double TubeVPitchEightNine
            {
                get => Default.TubeVPitchEightNine_63;
                set => Default.TubeVPitchEightNine_63 = value;
            }
            public double TubeVPitchNineTen
            {
                get => Default.TubeVPitchNineTen_63;
                set => Default.TubeVPitchNineTen_63 = value;
            }
            public double TubeVPitchTenEleven
            {
                get => Default.TubeVPitchTenEleven_63;
                set => Default.TubeVPitchTenEleven_63 = value;
            }
            public double TubeVPitchElevenTwelve
            {
                get => Default.TubeVPitchElevenTwelve_63;
                set => Default.TubeVPitchElevenTwelve_63 = value;
            }
            public bool IsBusted
            {
                get => Default.IsBusted_63;
                set => Default.IsBusted_63 = value;
            }
            public double StiffenerTHK
            {
                get => Default.StiffenerTHK_63;
                set => Default.StiffenerTHK_63 = value;
            }
            public double StiffenerWidth
            {
                get => Default.StiffenerWidth_63;
                set => Default.StiffenerWidth_63 = value;
            }
            public double StiffenerOffset
            {
                get => Default.StiffenerLength_63;
                set => Default.StiffenerLength_63 = value;
            }
            public double StiffenerBelowRow
            {
                get => Default.StiffenerBelowRow_63;
                set => Default.StiffenerBelowRow_63 = value;
            }
            public double StiffenerDistanceBelow
            {
                get => Default.StiffenerDistanceBelow_63;
                set => Default.StiffenerDistanceBelow_63 = value;
            }
            public int NumberOfRows
            {
                get
                {
                    return HeaderHelper.RowCounter(Header63);
                }
            }
            public double StiffenerWindowWidth
            {
                get => Default.StiffenerWindowWidth_63;
                set => Default.StiffenerWindowWidth_63 = value;
            }
            public double StiffenerWindowLength
            {
                get => Default.StiffenerWindowLength_63;
                set => Default.StiffenerWindowLength_63 = value;
            }
            public double StiffenerWindowQuantity
            {
                get => Default.StiffenerWindowQuantity_63;
                set => Default.StiffenerWindowQuantity_63 = value;
            }
            public double StiffenerWindowSpacing
            {
                get => Default.StiffenerWindowSpacing_63;
                set => Default.StiffenerWindowSpacing_63 = value;
            }
            public double StiffenerWindowOffset
            {
                get => Default.StiffenerWindowOffset_63;
                set => Default.StiffenerWindowOffset_63 = value;
            }
            public double PartitionTHK
            {
                get => Default.PartitionTHK_63;
                set => Default.PartitionTHK_63 = value;
            }
            public double PartitionWidth
            {
                get => Default.PartitionWidth_63;
                set => Default.PartitionWidth_63 = value;
            }
            public double PartitionBelowRow
            {
                get => Default.PartitionBelowRow_63;
                set => Default.PartitionBelowRow_63 = value;
            }
            public double PartitionDistanceBelow
            {
                get => Default.PartitionDistanceBelow_63;
                set => Default.PartitionDistanceBelow_63 = value;
            }
            public double EndPlateBustedSpan2
            {
                get => Default.EndPlateBustedSpan2_63;
                set => Default.EndPlateBustedSpan2_63 = value;
            }

        }
    }
}
