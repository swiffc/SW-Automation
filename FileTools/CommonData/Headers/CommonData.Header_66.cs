using FileTools.Base;
using static FileTools.Properties.Settings;

namespace FileTools.CommonData
{
    public static partial class CommonData 
    {// 66 Header
        public static Header_66 Header66 = new Header_66();
        public class Header_66 : IHeaderExtensions
        {
            public double BoxWidth 
            { 
                get => Default.BoxWidth66; 
                set => Default.BoxWidth66 = value; 
            }
            public double TubesheetTHK 
            { 
                get => Default.TubesheetTHK_66; 
                set => Default.TubesheetTHK_66 = value; 
            }
            public double PlugsheetTHK
            {
                get => Default.PlugsheetTHK_66;
                set => Default.PlugsheetTHK_66 = value;
            }
            public bool IsRequired
            {
                get => Default.IsRequired_66;
                set => Default.IsRequired_66 = value;
            }
            public double BoxHeight
            {
                get => Default.VerticalSpan66;
                set => Default.VerticalSpan66 = value;
            }
            public double BoxLength
            {
                get => Default.BoxLength66;
                set => Default.BoxLength66 = value;
            }
            public double TopAndBottomPlateTHK
            {
                get => Default.TopBottomTHK_66;
                set => Default.TopBottomTHK_66 = value;
            }
            public double TubeY 
            {
                get => Default.Y_Location66;
                set => Default.Y_Location66 = value;
            }
            public double TubesheetLength
            {
                get => Default.TubesheetLength_66;
                set => Default.TubesheetLength_66 = value;
            }
            public double TubesheetWidth
            {
                get => Default.TubesheetWidth_66;
                set => Default.TubesheetWidth_66 = value;
            }
            public double EndPlateTHK
            {
                get => Default.EndPlateTHK_66;
                set => Default.EndPlateTHK_66 = value;
            }
            public double TopBtmTHK
            {
                get => Default.TopBtmTHK_66;
                set => Default.TopBtmTHK_66 = value;
            }
            public double PlugsheetWidth
            {
                get => Default.PlugSheetWidth_66;
                set => Default.PlugSheetWidth_66 = value;
            }
            public double PlugsheetLength
            {
                get => Default.PlugSheetLength_66;
                set => Default.PlugSheetLength_66 = value;
            }
            public double EndPlateWidth
            {
                get => Default.EndPlateWidth_66;
                set => Default.EndPlateWidth_66 = value;
            }
            public double EndPlateLength
            {
                get => Default.EndPlateLength_66;
                set => Default.EndPlateLength_66 = value;
            }
            public double TopBtmWidth
            {
                get => Default.TopBtmWidth_66;
                set => Default.TopBtmWidth_66 = value;
            }
            public double TopBtmLength
            {
                get => Default.TopBtmLength_66;
                set => Default.TopBtmLength_66 = value;
            }
            public double TubeHoleDiameter
            {
                get => Default.TubeHoleDiameter_66;
                set => Default.TubeHoleDiameter_66 = value;
            }
            public double TubeEvenX
            {
                get => Default.TubeEvenX_66;
                set => Default.TubeEvenX_66 = value;
            }
            public double TubeOddX
            {
                get => Default.TubeOddX_66;
                set => Default.TubeOddX_66 = value;
            }
            public double TubeRow1Count
            {
                get => Default.TubeRow1Count_66;
                set => Default.TubeRow1Count_66 = value;
            }
            public double TubeRow2Count
            {
                get => Default.TubeRow2Count_66;
                set => Default.TubeRow2Count_66 = value;
            }
            public double TubeRow3Count
            {
                get => Default.TubeRow3Count_66;
                set => Default.TubeRow3Count_66 = value;
            }
            public double TubeRow4Count
            {
                get => Default.TubeRow4Count_66;
                set => Default.TubeRow4Count_66 = value;
            }
            public double TubeRow5Count
            {
                get => Default.TubeRow5Count_66;
                set => Default.TubeRow5Count_66 = value;
            }
            public double TubeRow6Count
            {
                get => Default.TubeRow6Count_66;
                set => Default.TubeRow6Count_66 = value;
            }
            public double TubeRow7Count
            {
                get => Default.TubeRow7Count_66;
                set => Default.TubeRow7Count_66 = value;
            }
            public double TubeRow8Count
            {
                get => Default.TubeRow8Count_66;
                set => Default.TubeRow8Count_66 = value;
            }
            public double TubeRow9Count
            {
                get => Default.TubeRow9Count_66;
                set => Default.TubeRow9Count_66 = value;
            }
            public double TubeRow10Count
            {
                get => Default.TubeRow10Count_66;
                set => Default.TubeRow10Count_66 = value;
            }
            public double TubeRow11Count
            {
                get => Default.TubeRow11Count_66;
                set => Default.TubeRow11Count_66 = value;
            }
            public double TubeRow12Count
            {
                get => Default.TubeRow12Count_66;
                set => Default.TubeRow12Count_66 = value;
            }
            public double TubeHPitchOdd
            {
                get => Default.TubeHPitchOdd_66;
                set => Default.TubeHPitchOdd_66 = value;
            }
            public double TubeHPitchEven
            {
                get => Default.TubeHPitchEven_66;
                set => Default.TubeHPitchEven_66 = value;
            }
            public double TubeVPitchOneTwo
            {
                get => Default.TubeVPitchOneTwo_66;
                set => Default.TubeVPitchOneTwo_66 = value;
            }
            public double TubeVPitchTwoThree
            {
                get => Default.TubeVPitchTwoThree_66;
                set => Default.TubeVPitchTwoThree_66 = value;
            }
            public double TubeVPitchThreeFour
            {
                get => Default.TubeVPitchThreeFour_66;
                set => Default.TubeVPitchThreeFour_66 = value;
            }
            public double TubeVPitchFourFive
            {
                get => Default.TubeVPitchFourFive_66;
                set => Default.TubeVPitchFourFive_66 = value;
            }
            public double TubeVPitchFiveSix
            {
                get => Default.TubeVPitchFiveSix_66;
                set => Default.TubeVPitchFiveSix_66 = value;
            }
            public double TubeVPitchSixSeven
            {
                get => Default.TubeVPitchSixSeven_66;
                set => Default.TubeVPitchSixSeven_66 = value;
            }
            public double TubeVPitchSevenEight
            {
                get => Default.TubeVPitchSevenEight_66;
                set => Default.TubeVPitchSevenEight_66 = value;
            }
            public double TubeVPitchEightNine
            {
                get => Default.TubeVPitchEightNine_66;
                set => Default.TubeVPitchEightNine_66 = value;
            }
            public double TubeVPitchNineTen
            {
                get => Default.TubeVPitchNineTen_66;
                set => Default.TubeVPitchNineTen_66 = value;
            }
            public double TubeVPitchTenEleven
            {
                get => Default.TubeVPitchTenEleven_66;
                set => Default.TubeVPitchTenEleven_66 = value;
            }
            public double TubeVPitchElevenTwelve
            {
                get => Default.TubeVPitchElevenTwelve_66;
                set => Default.TubeVPitchElevenTwelve_66 = value;
            }
            public bool IsBusted
            {
                get => Default.IsBusted_66;
                set => Default.IsBusted_66 = value;
            }

            public double StiffenerTHK
            {
                get => Default.StiffenerTHK_66;
                set => Default.StiffenerTHK_66 = value;
            }
            public double StiffenerWidth
            {
                get => Default.StiffenerWidth_66;
                set => Default.StiffenerWidth_66 = value;
            }
            public double StiffenerOffset
            {
                get => Default.StiffenerLength_66;
                set => Default.StiffenerLength_66 = value;
            }
            public double StiffenerBelowRow
            {
                get => Default.StiffenerBelowRow_66;
                set => Default.StiffenerBelowRow_66 = value;
            }
            public double StiffenerDistanceBelow
            {
                get => Default.StiffenerDistanceBelow_66;
                set => Default.StiffenerDistanceBelow_66 = value;
            }
            public int NumberOfRows
            {
                get
                {
                    return HeaderHelper.RowCounter(Header66);
                }
            }
            public double StiffenerWindowWidth
            {
                get => Default.StiffenerWindowWidth_66;
                set => Default.StiffenerWindowWidth_66 = value;
            }
            public double StiffenerWindowLength
            {
                get => Default.StiffenerWindowLength_66;
                set => Default.StiffenerWindowLength_66 = value;
            }
            public double StiffenerWindowQuantity
            {
                get => Default.StiffenerWindowQuantity_66;
                set => Default.StiffenerWindowQuantity_66 = value;
            }
            public double StiffenerWindowSpacing
            {
                get => Default.StiffenerWindowSpacing_66;
                set => Default.StiffenerWindowSpacing_66 = value;
            }
            public double StiffenerWindowOffset
            {
                get => Default.StiffenerWindowOffset_66;
                set => Default.StiffenerWindowOffset_66 = value;
            }
            public double PartitionTHK
            {
                get => Default.PartitionTHK_66;
                set => Default.PartitionTHK_66 = value;
            }
            public double PartitionWidth
            {
                get => Default.PartitionWidth_66;
                set => Default.PartitionWidth_66 = value;
            }
            public double PartitionBelowRow
            {
                get => Default.PartitionBelowRow_66;
                set => Default.PartitionBelowRow_66 = value;
            }

            public double PartitionDistanceBelow
            {
                get => Default.PartitionDistanceBelow_66;
                set => Default.PartitionDistanceBelow_66 = value;
            }
            public double EndPlateBustedSpan2
            {
                get => Default.EndPlateBustedSpan2_66;
                set => Default.EndPlateBustedSpan2_66 = value;
            }
            public double FootHeight
            {
                get => Default.FootHeight_66;
                set => Default.FootHeight_66 = value;
            }

            public string TubesheetPartNo
            {
                get => Default.TubesheetPartNo_66;
                set => Default.TubesheetPartNo_66 = value;
            }
            public string PlugsheetPartNo
            {
                get => Default.PlugsheetPartNo_66;
                set => Default.PlugsheetPartNo_66 = value;
            }
            public string TopBtmPartNo
            {
                get => Default.TopBtmPartNo_66;
                set => Default.TopBtmPartNo_66 = value;
            }
            public string EndPlatePartNo
            {
                get => Default.EndPlatePartNo_66;
                set => Default.EndPlatePartNo_66 = value;
            }
            public string EndPlatePartNo2
            {
                get => Default.EndPlatePartNo2_66;
                set => Default.EndPlatePartNo2_66 = value;
            }
            public string StiffenerPartNo
            {
                get => Default.StiffenerPartNo_66;
                set => Default.StiffenerPartNo_66 = value;
            }
            public string PartitionPartNo
            {
                get => Default.PartitionPartNo_66;
                set => Default.PartitionPartNo_66 = value;
            }
            public string FootPRLPartNo
            {
                get => Default.FootPRLPartNo_66;
                set => Default.FootPRLPartNo_66 = value;
            }
            public string FootPlatePartNo
            {
                get => Default.FootPlatePartNo_66;
                set => Default.FootPlatePartNo_66 = value;
            }
            public double PartitionTHK2
            {
                get => Default.PartitionTHK2_66;
                set => Default.PartitionTHK2_66 = value;
            }
            public string PartitionPartNo2
            {
                get => Default.PartitionPartNo2_66;
                set => Default.PartitionPartNo2_66 = value;
            }
            public double StiffenerTHK2
            {
                get => Default.StiffenerTHK2_66;
                set => Default.StiffenerTHK2_66 = value;
            }
            public string StiffenerPartNo2
            {
                get => Default.StiffenerPartNo2_66;
                set => Default.StiffenerPartNo2_66 = value;
            }


        }
    }
}
