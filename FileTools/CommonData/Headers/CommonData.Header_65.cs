using FileTools.Base;
using static FileTools.Properties.Settings;

namespace FileTools.CommonData
{
    public static partial class CommonData
    {// 65 Header
        public static Header_65 Header65 = new Header_65();
        public class Header_65 : IHeaderExtensions
        {
            public double BoxWidth
            {
                get => Default.BoxWidth65;
                set => Default.BoxWidth65 = value;
            }
            public double TubesheetTHK
            {
                get => Default.TubesheetTHK_65;
                set => Default.TubesheetTHK_65 = value;
            }
            public double PlugsheetTHK
            {
                get => Default.PlugsheetTHK_65;
                set => Default.PlugsheetTHK_65 = value;
            }
            public bool IsRequired
            {
                get => Default.IsRequired_65;
                set => Default.IsRequired_65 = value;
            }
            public double BoxHeight
            {
                get => Default.VerticalSpan65;
                set => Default.VerticalSpan65 = value;
            }
            public double BoxLength
            {
                get => Default.BoxLength65;
                set => Default.BoxLength65 = value;
            }
            public double TopAndBottomPlateTHK
            {
                get => Default.TopBottomTHK_65;
                set => Default.TopBottomTHK_65 = value;
            }
            public double TubeY
            {
                get => Default.Y_Location65;
                set => Default.Y_Location65 = value;
            }
            public double TubesheetLength
            {
                get => Default.TubesheetLength_65;
                set => Default.TubesheetLength_65 = value;
            }
            public double TubesheetWidth
            {
                get => Default.TubesheetWidth_65;
                set => Default.TubesheetWidth_65 = value;
            }
            public double EndPlateTHK
            {
                get => Default.EndPlateTHK_65;
                set => Default.EndPlateTHK_65 = value;
            }
            public double TopBtmTHK
            {
                get => Default.TopBtmTHK_65;
                set => Default.TopBtmTHK_65 = value;
            }
            public double PlugsheetWidth
            {
                get => Default.PlugSheetWidth_65;
                set => Default.PlugSheetWidth_65 = value;
            }
            public double PlugsheetLength
            {
                get => Default.PlugSheetLength_65;
                set => Default.PlugSheetLength_65 = value;
            }
            public double EndPlateWidth
            {
                get => Default.EndPlateWidth_65;
                set => Default.EndPlateWidth_65 = value;
            }
            public double EndPlateLength
            {
                get => Default.EndPlateLength_65;
                set => Default.EndPlateLength_65= value;
            }
            public double TopBtmWidth
            {
                get => Default.TopBtmWidth_65;
                set => Default.TopBtmWidth_65 = value;
            }
            public double TopBtmLength
            {
                get => Default.TopBtmLength_65;
                set => Default.TopBtmLength_65 = value;
            }
            public double TubeHoleDiameter
            {
                get => Default.TubeHoleDiameter_65;
                set => Default.TubeHoleDiameter_65 = value;
            }
            public double TubeEvenX
            {
                get => Default.TubeEvenX_65;
                set => Default.TubeEvenX_65 = value;
            }
            public double TubeOddX
            {
                get => Default.TubeOddX_65;
                set => Default.TubeOddX_65 = value;
            }
            public double TubeRow1Count
            {
                get => Default.TubeRow1Count_65;
                set => Default.TubeRow1Count_65 = value;
            }
            public double TubeRow2Count
            {
                get => Default.TubeRow2Count_65;
                set => Default.TubeRow2Count_65 = value;
            }
            public double TubeRow3Count
            {
                get => Default.TubeRow3Count_65;
                set => Default.TubeRow3Count_65 = value;
            }
            public double TubeRow4Count
            {
                get => Default.TubeRow4Count_65;
                set => Default.TubeRow4Count_65 = value;
            }
            public double TubeRow5Count
            {
                get => Default.TubeRow5Count_65;
                set => Default.TubeRow5Count_65 = value;
            }
            public double TubeRow6Count
            {
                get => Default.TubeRow6Count_65;
                set => Default.TubeRow6Count_65 = value;
            }
            public double TubeRow7Count
            {
                get => Default.TubeRow7Count_65;
                set => Default.TubeRow7Count_65 = value;
            }
            public double TubeRow8Count
            {
                get => Default.TubeRow8Count_65;
                set => Default.TubeRow8Count_65 = value;
            }
            public double TubeRow9Count
            {
                get => Default.TubeRow9Count_65;
                set => Default.TubeRow9Count_65 = value;
            }
            public double TubeRow10Count
            {
                get => Default.TubeRow10Count_65;
                set => Default.TubeRow10Count_65 = value;
            }
            public double TubeRow11Count
            {
                get => Default.TubeRow11Count_65;
                set => Default.TubeRow11Count_65 = value;
            }
            public double TubeRow12Count
            {
                get => Default.TubeRow12Count_65;
                set => Default.TubeRow12Count_65 = value;
            }
            public double TubeHPitchOdd
            {
                get => Default.TubeHPitchOdd_65;
                set => Default.TubeHPitchOdd_65 = value;
            }
            public double TubeHPitchEven
            {
                get => Default.TubeHPitchEven_65;
                set => Default.TubeHPitchEven_65 = value;
            }
            public double TubeVPitchOneTwo
            {
                get => Default.TubeVPitchOneTwo_65;
                set => Default.TubeVPitchOneTwo_65 = value;
            }
            public double TubeVPitchTwoThree
            {
                get => Default.TubeVPitchTwoThree_65;
                set => Default.TubeVPitchTwoThree_65 = value;
            }
            public double TubeVPitchThreeFour
            {
                get => Default.TubeVPitchThreeFour_65;
                set => Default.TubeVPitchThreeFour_65 = value;
            }
            public double TubeVPitchFourFive
            {
                get => Default.TubeVPitchFourFive_65;
                set => Default.TubeVPitchFourFive_65 = value;
            }
            public double TubeVPitchFiveSix
            {
                get => Default.TubeVPitchFiveSix_65;
                set => Default.TubeVPitchFiveSix_65 = value;
            }
            public double TubeVPitchSixSeven
            {
                get => Default.TubeVPitchSixSeven_65;
                set => Default.TubeVPitchSixSeven_65 = value;
            }
            public double TubeVPitchSevenEight
            {
                get => Default.TubeVPitchSevenEight_65;
                set => Default.TubeVPitchSevenEight_65 = value;
            }
            public double TubeVPitchEightNine
            {
                get => Default.TubeVPitchEightNine_65;
                set => Default.TubeVPitchEightNine_65 = value;
            }
            public double TubeVPitchNineTen
            {
                get => Default.TubeVPitchNineTen_65;
                set => Default.TubeVPitchNineTen_65 = value;
            }
            public double TubeVPitchTenEleven
            {
                get => Default.TubeVPitchTenEleven_65;
                set => Default.TubeVPitchTenEleven_65 = value;
            }
            public double TubeVPitchElevenTwelve
            {
                get => Default.TubeVPitchElevenTwelve_65;
                set => Default.TubeVPitchElevenTwelve_65 = value;
            }
            public bool IsBusted
            {
                get => Default.IsBusted_65;
                set => Default.IsBusted_65 = value;
            }

            public double StiffenerTHK
            {
                get => Default.StiffenerTHK_65;
                set => Default.StiffenerTHK_65 = value;
            }
            public double StiffenerWidth
            {
                get => Default.StiffenerWidth_65;
                set => Default.StiffenerWidth_65 = value;
            }
            public double StiffenerOffset
            {
                get => Default.StiffenerLength_65;
                set => Default.StiffenerLength_65 = value;
            }
            public double StiffenerBelowRow
            {
                get => Default.StiffenerBelowRow_65;
                set => Default.StiffenerBelowRow_65 = value;
            }
            public double StiffenerDistanceBelow
            {
                get => Default.StiffenerDistanceBelow_65;
                set => Default.StiffenerDistanceBelow_65 = value;
            }
            public int NumberOfRows
            {
                get
                {
                    return HeaderHelper.RowCounter(Header65);
                }
            }
            public double StiffenerWindowWidth
            {
                get => Default.StiffenerWindowWidth_65;
                set => Default.StiffenerWindowWidth_65 = value;
            }
            public double StiffenerWindowLength
            {
                get => Default.StiffenerWindowLength_65;
                set => Default.StiffenerWindowLength_65 = value;
            }
            public double StiffenerWindowQuantity
            {
                get => Default.StiffenerWindowQuantity_65;
                set => Default.StiffenerWindowQuantity_65 = value;
            }
            public double StiffenerWindowSpacing
            {
                get => Default.StiffenerWindowSpacing_65;
                set => Default.StiffenerWindowSpacing_65 = value;
            }
            public double StiffenerWindowOffset
            {
                get => Default.StiffenerWindowOffset_65;
                set => Default.StiffenerWindowOffset_65 = value;
            }
            public double PartitionTHK
            {
                get => Default.PartitionTHK_65;
                set => Default.PartitionTHK_65 = value;
            }
            public double PartitionWidth
            {
                get => Default.PartitionWidth_65;
                set => Default.PartitionWidth_65 = value;
            }
            public double PartitionBelowRow
            {
                get => Default.PartitionBelowRow_65;
                set => Default.PartitionBelowRow_65 = value;
            }
            public double PartitionDistanceBelow
            {
                get => Default.PartitionDistanceBelow_65;
                set => Default.PartitionDistanceBelow_65 = value;
            }
            public double EndPlateBustedSpan2
            {
                get => Default.EndPlateBustedSpan2_65;
                set => Default.EndPlateBustedSpan2_65 = value;
            }
            public double FootHeight
            {
                get => Default.FootHeight_65;
                set => Default.FootHeight_65 = value;
            }
            public string TubesheetPartNo
            {
                get => Default.TubesheetPartNo_65;
                set => Default.TubesheetPartNo_65 = value;
            }
            public string PlugsheetPartNo
            {
                get => Default.PlugsheetPartNo_65;
                set => Default.PlugsheetPartNo_65 = value;
            }
            public string TopBtmPartNo
            {
                get => Default.TopBtmPartNo_65;
                set => Default.TopBtmPartNo_65 = value;
            }
            public string EndPlatePartNo
            {
                get => Default.EndPlatePartNo_65;
                set => Default.EndPlatePartNo_65 = value;
            }
            public string EndPlatePartNo2
            {
                get => Default.EndPlatePartNo2_65;
                set => Default.EndPlatePartNo2_65 = value;
            }
            public string StiffenerPartNo
            {
                get => Default.StiffenerPartNo_65;
                set => Default.StiffenerPartNo_65 = value;
            }
            public string PartitionPartNo
            {
                get => Default.PartitionPartNo_65;
                set => Default.PartitionPartNo_65 = value;
            }
            public string FootPRLPartNo
            {
                get => Default.FootPRLPartNo_65;
                set => Default.FootPRLPartNo_65 = value;
            }
            public string FootPlatePartNo
            {
                get => Default.FootPlatePartNo_65;
                set => Default.FootPlatePartNo_65 = value;
            }
            public double PartitionTHK2
            {
                get => Default.PartitionTHK2_65;
                set => Default.PartitionTHK2_65 = value;
            }
            public string PartitionPartNo2
            {
                get => Default.PartitionPartNo2_65;
                set => Default.PartitionPartNo2_65 = value;
            }
            public double StiffenerTHK2
            {
                get => Default.StiffenerTHK2_65;
                set => Default.StiffenerTHK2_65 = value;
            }
            public string StiffenerPartNo2
            {
                get => Default.StiffenerPartNo2_65;
                set => Default.StiffenerPartNo2_65 = value;
            }
            public double PartitionBelowRow2
            {
                get => Default.PartitionBelowRow2_65;
                set => Default.PartitionBelowRow2_65 = value;
            }
            public double PartitionDistanceBelow2
            {
                get => Default.PartitionDistanceBelow2_65;
                set => Default.PartitionDistanceBelow2_65 = value;
            }
            public double StiffenerBelowRow2
            {
                get => Default.StiffenerBelowRow2_65;
                set => Default.StiffenerBelowRow2_65 = value;
            }
            public double StiffenerDistanceBelow2
            {
                get => Default.StiffenerDistanceBelow2_65;
                set => Default.StiffenerDistanceBelow2_65 = value;
            }
            public double PartitionBelowRow3
            {
                get => Default.PartitionBelowRow3_65;
                set => Default.PartitionBelowRow3_65 = value;
            }
            public double PartitionDistanceBelow3
            {
                get => Default.PartitionDistanceBelow3_65;
                set => Default.PartitionDistanceBelow3_65 = value;
            }
            public double StiffenerBelowRow3
            {
                get => Default.StiffenerBelowRow3_65;
                set => Default.StiffenerBelowRow3_65 = value;
            }
            public double StiffenerDistanceBelow3
            {
                get => Default.StiffenerDistanceBelow3_65;
                set => Default.StiffenerDistanceBelow3_65 = value;
            }
            public double WetLocationY
            {
                get => Default.WetLocationY_65;
                set => Default.WetLocationY_65 = value;
            }
        }
    }
}
