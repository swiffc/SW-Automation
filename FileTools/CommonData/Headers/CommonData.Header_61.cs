using FileTools.Base;
using static FileTools.Properties.Settings;

namespace FileTools.CommonData
{
    public static partial class CommonData 
    {// 61 Header
        public static Header_61 Header61 = new Header_61();
        public class Header_61 : IHeaderExtensions
        {
            public double BoxWidth 
            { 
                get => Default.BoxWidth61; 
                set => Default.BoxWidth61 = value; 
            }
            public double TubesheetTHK 
            { 
                get => Default.TubesheetTHK_61; 
                set => Default.TubesheetTHK_61 = value; 
            }
            public double PlugsheetTHK
            {
                get => Default.PlugsheetTHK_61;
                set => Default.PlugsheetTHK_61 = value;
            }
            public bool IsRequired
            {
                get => Default.IsRequired_61;
                set => Default.IsRequired_61 = value;
            }
            public double BoxHeight 
            {
                get => Default.VerticalSpan61; 
                set => Default.VerticalSpan61 = value;
            }
            public double BoxLength 
            { 
                get => Default.BoxLength61; 
                set => Default.BoxLength61 = value; 
            }
            public double TopAndBottomPlateTHK 
            { 
                get => Default.TopBottomTHK_61; 
                set => Default.TopBottomTHK_61 = value; 
            }
            public double TubeY
            {
                get => Default.Y_Location61;
                set => Default.Y_Location61 = value;
            }
            public double TubeOddX 
            { 
                get => Default.Xtop_61; 
                set => Default.Xtop_61 = value; 
            }
            public double TubesheetLength
            {
                get => Default.TubesheetLength_61;
                set => Default.TubesheetLength_61 = value;
            }
            public double TubesheetWidth
            {
                get => Default.TubesheetWidth_61;
                set => Default.TubesheetWidth_61 = value;
            }
            public double EndPlateTHK
            {
                get => Default.EndPlateTHK_61;
                set => Default.EndPlateTHK_61 = value;
            }
            public double TopBtmTHK
            {
                get => Default.TopBtmTHK_61;
                set => Default.TopBtmTHK_61 = value;
            }
            public double PlugsheetWidth
            {
                get => Default.PlugSheetWidth_61;
                set => Default.PlugSheetWidth_61 = value;
            }
            public double PlugsheetLength
            {
                get => Default.PlugSheetLength_61;
                set => Default.PlugSheetLength_61 = value;
            }
            public double EndPlateWidth
            {
                get => Default.EndPlateWidth_61;
                set => Default.EndPlateWidth_61 = value;
            }
            public double EndPlateLength
            {
                get => Default.EndPlateLength_61;
                set => Default.EndPlateLength_61 = value;
            }
            public double TopBtmWidth
            {
                get => Default.TopBtmWidth_61;
                set => Default.TopBtmWidth_61 = value;
            }
            public double TopBtmLength
            {
                get => Default.TopBtmLength_61;
                set => Default.TopBtmLength_61 = value;
            }
            public double TubeHoleDiameter 
            { 
                get => Default.TubeHoleDiameter_61;
                set => Default.TubeHoleDiameter_61 = value; 
            }
            public double TubeEvenX
            {
                get => Default.TubeEvenX_61;
                set => Default.TubeEvenX_61 = value;
            }
            public double TubeRow1Count
            {
                get => Default.TubeRow1Count_61;
                set => Default.TubeRow1Count_61 = value;
            }
            public double TubeRow2Count
            {
                get => Default.TubeRow2Count_61;
                set => Default.TubeRow2Count_61 = value;
            }
            public double TubeRow3Count
            {
                get => Default.TubeRow3Count_61;
                set => Default.TubeRow3Count_61 = value;
            }
            public double TubeRow4Count
            {
                get => Default.TubeRow4Count_61;
                set => Default.TubeRow4Count_61 = value;
            }
            public double TubeRow5Count
            {
                get => Default.TubeRow5Count_61;
                set => Default.TubeRow5Count_61 = value;
            }
            public double TubeRow6Count
            {
                get => Default.TubeRow6Count_61;
                set => Default.TubeRow6Count_61 = value;
            }
            public double TubeRow7Count
            {
                get => Default.TubeRow7Count_61;
                set => Default.TubeRow7Count_61 = value;
            }
            public double TubeRow8Count
            {
                get => Default.TubeRow8Count_61;
                set => Default.TubeRow8Count_61 = value;
            }
            public double TubeRow9Count
            {
                get => Default.TubeRow9Count_61;
                set => Default.TubeRow9Count_61 = value;
            }
            public double TubeRow10Count
            {
                get => Default.TubeRow10Count_61;
                set => Default.TubeRow10Count_61 = value;
            }
            public double TubeRow11Count
            {
                get => Default.TubeRow11Count_61;
                set => Default.TubeRow11Count_61 = value;
            }
            public double TubeRow12Count
            {
                get => Default.TubeRow12Count_61;
                set => Default.TubeRow12Count_61 = value;
            }
            public double TubeHPitchOdd
            {
                get => Default.TubeHPitchOdd_61;
                set => Default.TubeHPitchOdd_61 = value;
            }
            public double TubeHPitchEven
            {
                get => Default.TubeHPitchEven_61;
                set => Default.TubeHPitchEven_61 = value;
            }

            public double TubeVPitchOneTwo
            {
                get => Default.TubeVPitchOneTwo_61;
                set => Default.TubeVPitchOneTwo_61 = value;
            }
            public double TubeVPitchTwoThree
            {
                get => Default.TubeVPitchTwoThree_61;
                set => Default.TubeVPitchTwoThree_61 = value;
            }
            public double TubeVPitchThreeFour
            {
                get => Default.TubeVPitchThreeFour_61;
                set => Default.TubeVPitchThreeFour_61 = value;
            }
            public double TubeVPitchFourFive
            {
                get => Default.TubeVPitchFourFive_61;
                set => Default.TubeVPitchFourFive_61 = value;
            }
            public double TubeVPitchFiveSix
            {
                get => Default.TubeVPitchFiveSix_61;
                set => Default.TubeVPitchFiveSix_61 = value;
            }
            public double TubeVPitchSixSeven
            {
                get => Default.TubeVPitchSixSeven_61;
                set => Default.TubeVPitchSixSeven_61 = value;
            }
            public double TubeVPitchSevenEight
            {
                get => Default.TubeVPitchSevenEight_61;
                set => Default.TubeVPitchSevenEight_61 = value;
            }
            public double TubeVPitchEightNine
            {
                get => Default.TubeVPitchEightNine_61;
                set => Default.TubeVPitchEightNine_61 = value;
            }
            public double TubeVPitchNineTen
            {
                get => Default.TubeVPitchNineTen_61;
                set => Default.TubeVPitchNineTen_61 = value;
            }
            public double TubeVPitchTenEleven
            {
                get => Default.TubeVPitchTenEleven_61;
                set => Default.TubeVPitchTenEleven_61 = value;
            }
            public double TubeVPitchElevenTwelve
            {
                get => Default.TubeVPitchElevenTwelve_61;
                set => Default.TubeVPitchElevenTwelve_61 = value;
            }
            public bool IsBusted
            {
                get => Default.IsBusted_61;
                set => Default.IsBusted_61 = value;
            }
            public double StiffenerTHK
            {
                get => Default.StiffenerTHK_61;
                set => Default.StiffenerTHK_61 = value;
            }
            public double StiffenerWidth
            {
                get => Default.StiffenerWidth_61;
                set => Default.StiffenerWidth_61 = value;
            }
            public double StiffenerOffset
            {
                get => Default.StiffenerLength_61;
                set => Default.StiffenerLength_61 = value;
            }
            public double StiffenerBelowRow
            {
                get => Default.StiffenerBelowRow_61;
                set => Default.StiffenerBelowRow_61 = value;
            }
            public double StiffenerDistanceBelow
            {
                get => Default.StiffenerDistanceBelow_61;
                set => Default.StiffenerDistanceBelow_61 = value;
            }
            public int NumberOfRows 
            { 
                get
                {
                    return HeaderHelper.RowCounter(Header61);
                }
            }
            public double StiffenerWindowWidth
            {
                get => Default.StiffenerWindowWidth_61;
                set => Default.StiffenerWindowWidth_61 = value;
            }
            public double StiffenerWindowLength
            {
                get => Default.StiffenerWindowLength_61;
                set => Default.StiffenerWindowLength_61 = value;
            }
            public double StiffenerWindowQuantity
            {
                get => Default.StiffenerWindowQuantity_61;
                set => Default.StiffenerWindowQuantity_61 = value;
            }
            public double StiffenerWindowSpacing
            {
                get => Default.StiffenerWindowSpacing_61;
                set => Default.StiffenerWindowSpacing_61 = value;
            }
            public double StiffenerWindowOffset
            {
                get => Default.StiffenerWindowOffset_61;
                set => Default.StiffenerWindowOffset_61 = value;
            }
            public double PartitionTHK
            {
                get => Default.PartitionTHK_61;
                set => Default.PartitionTHK_61 = value;
            }
            public double PartitionWidth
            {
                get => Default.PartitionWidth_61;
                set => Default.PartitionWidth_61 = value;
            }
            public double PartitionBelowRow
            {
                get => Default.PartitionBelowRow_61;
                set => Default.PartitionBelowRow_61 = value;
            }
            public double PartitionDistanceBelow
            {
                get => Default.PartitionDistanceBelow_61;
                set => Default.PartitionDistanceBelow_61 = value;
            }
            public double EndPlateBustedSpan2
            {
                get => Default.EndPlateBustedSpan2_61;
                set => Default.EndPlateBustedSpan2_61 = value;
            }
            public double FootHeight
            {
                get => Default.FootHeight_61;
                set => Default.FootHeight_61 = value;
            }
            public string TubesheetPartNo
            {
                get => Default.TubesheetPartNo_61;
                set => Default.TubesheetPartNo_61 = value;
            }
            public string PlugsheetPartNo
            {
                get => Default.PlugsheetPartNo_61;
                set => Default.PlugsheetPartNo_61 = value;
            }
            public string TopBtmPartNo
            {
                get => Default.TopBtmPartNo_61;
                set => Default.TopBtmPartNo_61 = value;
            }
            public string EndPlatePartNo
            {
                get => Default.EndPlatePartNo_61;
                set => Default.EndPlatePartNo_61 = value;
            }
            public string EndPlatePartNo2
            {
                get => Default.EndPlatePartNo2_61;
                set => Default.EndPlatePartNo2_61 = value;
            }
            public string StiffenerPartNo
            {
                get => Default.StiffenerPartNo_61;
                set => Default.StiffenerPartNo_61 = value;
            }
            public string PartitionPartNo
            {
                get => Default.PartitionPartNo_61;
                set => Default.PartitionPartNo_61 = value;
            }
            public string FootPRLPartNo
            {
                get => Default.FootPRLPartNo_61;
                set => Default.FootPRLPartNo_61 = value;
            }
            public string FootPlatePartNo
            {
                get => Default.FootPlatePartNo_61;
                set => Default.FootPlatePartNo_61 = value;
            }
            public double PartitionTHK2
            {
                get => Default.PartitionTHK2_61;
                set => Default.PartitionTHK2_61 = value;
            }
            public string PartitionPartNo2
            {
                get => Default.PartitionPartNo2_61;
                set => Default.PartitionPartNo2_61 = value;
            }
            public double StiffenerTHK2
            {
                get => Default.StiffenerTHK2_61;
                set => Default.StiffenerTHK2_61 = value;
            }
            public string StiffenerPartNo2
            {
                get => Default.StiffenerPartNo2_61;
                set => Default.StiffenerPartNo2_61 = value;
            }
            public double PartitionBelowRow2
            {
                get => Default.PartitionBelowRow2_61;
                set => Default.PartitionBelowRow2_61 = value;
            }
            public double PartitionDistanceBelow2
            {
                get => Default.PartitionDistanceBelow2_61;
                set => Default.PartitionDistanceBelow2_61 = value;
            }
            public double StiffenerBelowRow2
            {
                get => Default.StiffenerBelowRow2_61;
                set => Default.StiffenerBelowRow2_61 = value;
            }
            public double StiffenerDistanceBelow2
            {
                get => Default.StiffenerDistanceBelow2_61;
                set => Default.StiffenerDistanceBelow2_61 = value;
            }
            public double PartitionBelowRow3
            {
                get => Default.PartitionBelowRow3_61;
                set => Default.PartitionBelowRow3_61 = value;
            }
            public double PartitionDistanceBelow3
            {
                get => Default.PartitionDistanceBelow3_61;
                set => Default.PartitionDistanceBelow3_61 = value;
            }
            public double StiffenerBelowRow3
            {
                get => Default.StiffenerBelowRow3_61;
                set => Default.StiffenerBelowRow3_61 = value;
            }
            public double StiffenerDistanceBelow3
            {
                get => Default.StiffenerDistanceBelow3_61;
                set => Default.StiffenerDistanceBelow3_61 = value;
            }
        }

    }
}
