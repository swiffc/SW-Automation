using FileTools.Base;
using Microsoft.Office.Interop.Excel;
using System;
using System.Collections.Generic;
using System.Reflection;
using System.Windows.Forms;
using static Excel.Prego;
using CheckBox = System.Windows.Forms.CheckBox;
using TextBox = System.Windows.Forms.TextBox;
using static Excel.StaticHelpers;
using static FileTools.CommonData.CommonData;

namespace Excel
{
    public class Header_DataManager
    {
        // Data transfer objects
        public class Prego_DTO
        {
            public (string[] Cells, Worksheet Sheet) HeaderRequired { get; set; }
            public (string[] Cells, Worksheet Sheet) BoxWidth { get; set; }
            public (string[] Cells, Worksheet Sheet) TubesheetTHK { get; set; }
            public (string[] Cells, Worksheet Sheet) TubesheetLength { get; set; }
            public (string[] Cells, Worksheet Sheet) TubesheetWidth { get; set; }
            public (string[] Cells, Worksheet Sheet) PlugsheetTHK { get; set; }
            public (string[] Cells, Worksheet Sheet) TopAndBottomPlateTHK { get; set; }
            public (string[] Cells, Worksheet Sheet) BoxHeight { get; set; }
            public (string[] Cells, Worksheet Sheet) BoxLength { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeY { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeOddX { get; set; }
            public (string[] Cells, Worksheet Sheet) EndPlateTHK { get; set; }
            public (string[] Cells, Worksheet Sheet) TopBtmTHK { get; set; }
            public (string[] Cells, Worksheet Sheet) PlugsheetLength { get; set; }
            public (string[] Cells, Worksheet Sheet) PlugsheetWidth { get; set; }
            public (string[] Cells, Worksheet Sheet) TopBtmLength { get; set; }
            public (string[] Cells, Worksheet Sheet) TopBtmWidth { get; set; }
            public (string[] Cells, Worksheet Sheet) EndPlateLength { get; set; }
            public (string[] Cells, Worksheet Sheet) EndPlateWidth { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeHoleDiameter { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeEvenX { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeRow1Count { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeRow2Count { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeRow3Count { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeRow4Count { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeRow5Count { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeRow6Count { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeRow7Count { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeRow8Count { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeRow9Count { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeRow10Count { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeRow11Count { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeRow12Count { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeHPitchOdd { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeHPitchEven { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeVPitchOneTwo { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeVPitchTwoThree { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeVPitchThreeFour { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeVPitchFourFive { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeVPitchFiveSix { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeVPitchSixSeven { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeVPitchSevenEight { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeVPitchEightNine { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeVPitchNineTen { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeVPitchTenEleven { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeVPitchElevenTwelve { get; set; }
            public (string[] Cells, Worksheet Sheet) StiffenerTHK { get; set; }
            public (string[] Cells, Worksheet Sheet) StiffenerWidth { get; set; }
            public (string[] Cells, Worksheet Sheet) StiffenerOffset { get; set; }
            public (string[] Cells, Worksheet Sheet) StiffenerBelowRow { get; set; }
            public (string[] Cells, Worksheet Sheet) StiffenerDistanceBelow { get; set; }
            public (string[] Cells, Worksheet Sheet) StiffenerWindowWidth { get; set; }
            public (string[] Cells, Worksheet Sheet) StiffenerWindowLength { get; set; }
            public (string[] Cells, Worksheet Sheet) StiffenerWindowQuantity { get; set; }
            public (string[] Cells, Worksheet Sheet) StiffenerWindowSpacing { get; set; }
            public (string[] Cells, Worksheet Sheet) StiffenerWindowOffset { get; set; }
            public (string[] Cells, Worksheet Sheet) PartitionTHK { get; set; }
            public (string[] Cells, Worksheet Sheet) PartitionWidth { get; set; }
            public (string[] Cells, Worksheet Sheet) PartitionBelowRow { get; set; }
            public (string[] Cells, Worksheet Sheet) PartitionDistanceBelow { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeTopOD { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeTopID { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeTopQ { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeTopRD { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeTopYY { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeTopX { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeTopR { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeTopO { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeTopBC { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeTopNB { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeTopDB { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeTopOffsetX { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeTopOffsetY { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeTopCount { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeTopSpacing { get; set; }
        }


        public static Dictionary<string, UI_DTO> HeaderAppData;
        public class UI_DTO
        {
            public IHeaderExtensions Header { get; set; }
            public CheckBox Enabled { get; set; }
            public TextBox BoxWidthTextBox { get; set; }
            public TextBox TubesheetTHKTextBox { get; set; }
            public TextBox TubesheetLengthTextBox { get; set; }
            public TextBox TubesheetWidthTextBox { get; set; }
            public TextBox PlugsheetTHKTextBox { get; set; }
            public TextBox TopAndBottomPlateTHKTextBox { get; set; }
            public TextBox BoxLengthTextBox { get; set; }
            public TextBox BoxHeightTextBox { get; set; }
            public TextBox TubeYTextBox { get; set; }
            public TextBox TubeOddXTextBox { get; set; }
            public TextBox EndPlateTHKTextBox { get; set; }
            public TextBox TopBtmTHKTextBox { get; set; }
            public TextBox PlugsheetLengthTextBox { get; set; }
            public TextBox PlugsheetWidthTextBox { get; set; }
            public TextBox TopBtmLengthTextBox { get; set; }
            public TextBox TopBtmWidthTextBox { get; set; }
            public TextBox EndPlateLengthTextBox { get; set; }
            public TextBox EndPlateWidthTextBox { get; set; }
            public TextBox TubeHoleDiameterTextBox { get; set; }
            public TextBox TubeEvenXTextBox { get; set; }
            public TextBox TubeRow1CountTextBox { get; set; }
            public TextBox TubeRow2CountTextBox { get; set; }
            public TextBox TubeRow3CountTextBox { get; set; }
            public TextBox TubeRow4CountTextBox { get; set; }
            public TextBox TubeRow5CountTextBox { get; set; }
            public TextBox TubeRow6CountTextBox { get; set; }
            public TextBox TubeRow7CountTextBox { get; set; }
            public TextBox TubeRow8CountTextBox { get; set; }
            public TextBox TubeRow9CountTextBox { get; set; }
            public TextBox TubeRow10CountTextBox { get; set; }
            public TextBox TubeRow11CountTextBox { get; set; }
            public TextBox TubeRow12CountTextBox { get; set; }
            public TextBox TubeHPitchOddTextBox { get; set; }
            public TextBox TubeHPitchEvenTextBox { get; set; }
            public TextBox TubeVPitchOneTwoTextBox { get; set; }
            public TextBox TubeVPitchTwoThreeTextBox { get; set; }
            public TextBox TubeVPitchThreeFourTextBox { get; set; }
            public TextBox TubeVPitchFourFiveTextBox { get; set; }
            public TextBox TubeVPitchFiveSixTextBox { get; set; }
            public TextBox TubeVPitchSixSevenTextBox { get; set; }
            public TextBox TubeVPitchSevenEightTextBox { get; set; }
            public TextBox TubeVPitchEightNineTextBox { get; set; }
            public TextBox TubeVPitchNineTenTextBox { get; set; }
            public TextBox TubeVPitchTenElevenTextBox { get; set; }
            public TextBox TubeVPitchElevenTwelveTextBox { get; set; }
            public TextBox StiffenerTHKTextBox { get; set; }
            public TextBox StiffenerWidthTextBox { get; set; }
            public TextBox StiffenerOffsetTextBox { get; set; }
            public TextBox StiffenerBelowRowTextBox { get; set; }
            public TextBox StiffenerDistanceBelowTextBox { get; set; }
            public TextBox StiffenerWindowWidthTextBox { get; set; }
            public TextBox StiffenerWindowLengthTextBox { get; set; }
            public TextBox StiffenerWindowQuantityTextBox { get; set; }
            public TextBox StiffenerWindowSpacingTextBox { get; set; }
            public TextBox StiffenerWindowOffsetTextBox { get; set; }
            public TextBox PartitionTHKTextBox { get; set; }
            public TextBox PartitionWidthTextBox { get; set; }
            public TextBox PartitionBelowRowTextBox { get; set; }
            public TextBox PartitionDistanceBelowTextBox { get; set; }
            public TextBox FlangeTopODTextBox { get; set; }
            public TextBox FlangeTopIDTextBox { get; set; }
            public TextBox FlangeTopQTextBox { get; set; }
            public TextBox FlangeTopRDTextBox { get; set; }
            public TextBox FlangeTopYYTextBox { get; set; }
            public TextBox FlangeTopXTextBox { get; set; }
            public TextBox FlangeTopRTextBox { get; set; }
            public TextBox FlangeTopOTextBox { get; set; }
            public TextBox FlangeTopBCTextBox { get; set; }
            public TextBox FlangeTopNBTextBox { get; set; }
            public TextBox FlangeTopDBTextBox { get; set; }
            public TextBox FlangeTopOffsetXTextBox { get; set; }
            public TextBox FlangeTopOffsetYTextBox { get; set; }
            public TextBox FlangeTopCountTextBox { get; set; }
            public TextBox FlangeTopSpacingTextBox { get; set; }
            public TextBox EndPlateBustedSpan2TextBox { get; set; }
        }


        public static void MapLocal_UI_To_DTO(Form formInstance)
        {
            HeaderAppData = new Dictionary<string, UI_DTO>();

            for (int i = 61; i <= 66; i++)
            {
                var uiDto = new UI_DTO
                {
                    Header = GetHeader(i),
                    Enabled = GetControl<CheckBox>(formInstance, "cEnabled", i),
                    BoxWidthTextBox = GetControl<TextBox>(formInstance, "tBoxWidth_", i),
                    TubesheetTHKTextBox = GetControl<TextBox>(formInstance, "tTubesheetTHK_", i),
                    TubesheetLengthTextBox = GetControl<TextBox>(formInstance, "tTubesheetLength_", i),
                    TubesheetWidthTextBox = GetControl<TextBox>(formInstance, "tTubesheetWidth_", i),
                    PlugsheetTHKTextBox = GetControl<TextBox>(formInstance, "tPlugsheetTHK_", i),
                    TopAndBottomPlateTHKTextBox = GetControl<TextBox>(formInstance, "tTopBottomTHK_", i),
                    BoxLengthTextBox = GetControl<TextBox>(formInstance, "tBoxLength_", i),
                    BoxHeightTextBox = GetControl<TextBox>(formInstance, "tBoxHeight_", i),
                    EndPlateTHKTextBox = GetControl<TextBox>(formInstance, "tEndPlateTHK_", i),
                    TopBtmTHKTextBox = GetControl<TextBox>(formInstance, "tTopBtmTHK_", i),
                    PlugsheetLengthTextBox = GetControl<TextBox>(formInstance, "tPlugsheetLength_", i),
                    PlugsheetWidthTextBox = GetControl<TextBox>(formInstance, "tPlugsheetWidth_", i),
                    TopBtmLengthTextBox = GetControl<TextBox>(formInstance, "tTopBtmLength_", i),
                    TopBtmWidthTextBox = GetControl<TextBox>(formInstance, "tTopBtmWidth_", i),
                    EndPlateLengthTextBox = GetControl<TextBox>(formInstance, "tEndPlateLength_", i),
                    EndPlateWidthTextBox = GetControl<TextBox>(formInstance, "tEndPlateWidth_", i),
                    TubeHoleDiameterTextBox = GetControl<TextBox>(formInstance, "tTubeHoleDiameter_", i),
                    TubeEvenXTextBox = GetControl<TextBox>(formInstance, "tTubeEvenX_", i),
                    TubeYTextBox = GetControl<TextBox>(formInstance, "tTubeY_", i),
                    TubeOddXTextBox = GetControl<TextBox>(formInstance, "tTubeOddX_", i),
                    TubeRow1CountTextBox = GetControl<TextBox>(formInstance, "tTubeRow1Count_", i),
                    TubeRow2CountTextBox = GetControl<TextBox>(formInstance, "tTubeRow2Count_", i),
                    TubeRow3CountTextBox = GetControl<TextBox>(formInstance, "tTubeRow3Count_", i),
                    TubeRow4CountTextBox = GetControl<TextBox>(formInstance, "tTubeRow4Count_", i),
                    TubeRow5CountTextBox = GetControl<TextBox>(formInstance, "tTubeRow5Count_", i),
                    TubeRow6CountTextBox = GetControl<TextBox>(formInstance, "tTubeRow6Count_", i),
                    TubeRow7CountTextBox = GetControl<TextBox>(formInstance, "tTubeRow7Count_", i),
                    TubeRow8CountTextBox = GetControl<TextBox>(formInstance, "tTubeRow8Count_", i),
                    TubeRow9CountTextBox = GetControl<TextBox>(formInstance, "tTubeRow9Count_", i),
                    TubeRow10CountTextBox = GetControl<TextBox>(formInstance, "tTubeRow10Count_", i),
                    TubeRow11CountTextBox = GetControl<TextBox>(formInstance, "tTubeRow11Count_", i),
                    TubeRow12CountTextBox = GetControl<TextBox>(formInstance, "tTubeRow12Count_", i),
                    TubeHPitchOddTextBox = GetControl<TextBox>(formInstance, "tTubeHPitchOdd_", i),
                    TubeHPitchEvenTextBox = GetControl<TextBox>(formInstance, "tTubeHPitchEven_", i),
                    TubeVPitchOneTwoTextBox = GetControl<TextBox>(formInstance, "tTubeVPitchOneTwo_", i),
                    TubeVPitchTwoThreeTextBox = GetControl<TextBox>(formInstance, "tTubeVPitchTwoThree_", i),
                    TubeVPitchThreeFourTextBox = GetControl<TextBox>(formInstance, "tTubeVPitchThreeFour_", i),
                    TubeVPitchFourFiveTextBox = GetControl<TextBox>(formInstance, "tTubeVPitchFourFive_", i),
                    TubeVPitchFiveSixTextBox = GetControl<TextBox>(formInstance, "tTubeVPitchFiveSix_", i),
                    TubeVPitchSixSevenTextBox = GetControl<TextBox>(formInstance, "tTubeVPitchSixSeven_", i),
                    TubeVPitchSevenEightTextBox = GetControl<TextBox>(formInstance, "tTubeVPitchSevenEight_", i),
                    TubeVPitchEightNineTextBox = GetControl<TextBox>(formInstance, "tTubeVPitchEightNine_", i),
                    TubeVPitchNineTenTextBox = GetControl<TextBox>(formInstance, "tTubeVPitchNineTen_", i),
                    TubeVPitchTenElevenTextBox = GetControl<TextBox>(formInstance, "tTubeVPitchTenEleven_", i),
                    TubeVPitchElevenTwelveTextBox = GetControl<TextBox>(formInstance, "tTubeVPitchElevenTwelve_", i),
                    StiffenerTHKTextBox = GetControl<TextBox>(formInstance, "tStiffenerTHK_", i),
                    StiffenerWidthTextBox = GetControl<TextBox>(formInstance, "tStiffenerWidth_", i),
                    StiffenerOffsetTextBox = GetControl<TextBox>(formInstance, "tStiffenerOffset_", i),
                    StiffenerBelowRowTextBox = GetControl<TextBox>(formInstance, "tStiffenerBelowRow_", i),
                    StiffenerDistanceBelowTextBox = GetControl<TextBox>(formInstance, "tStiffenerDistanceBelow_", i),
                    StiffenerWindowWidthTextBox = GetControl<TextBox>(formInstance, "tStiffenerWindowWidth_", i),
                    StiffenerWindowLengthTextBox = GetControl<TextBox>(formInstance, "tStiffenerWindowLength_", i),
                    StiffenerWindowQuantityTextBox = GetControl<TextBox>(formInstance, "tStiffenerWindowQuantity_", i),
                    StiffenerWindowSpacingTextBox = GetControl<TextBox>(formInstance, "tStiffenerWindowSpacing_", i),
                    StiffenerWindowOffsetTextBox = GetControl<TextBox>(formInstance, "tStiffenerWindowOffset_", i),
                    PartitionTHKTextBox = GetControl<TextBox>(formInstance, "tPartitionTHK_", i),
                    PartitionWidthTextBox = GetControl<TextBox>(formInstance, "tPartitionWidth_", i),
                    PartitionBelowRowTextBox = GetControl<TextBox>(formInstance, "tPartitionBelowRow_", i),
                    PartitionDistanceBelowTextBox = GetControl<TextBox>(formInstance, "tPartitionDistanceBelow_", i),
                    FlangeTopODTextBox = GetControl<TextBox>(formInstance, "tFlangeTopOD_", i),
                    FlangeTopIDTextBox = GetControl<TextBox>(formInstance, "tFlangeTopID_", i),
                    FlangeTopQTextBox = GetControl<TextBox>(formInstance, "tFlangeTopQ_", i),
                    FlangeTopRDTextBox = GetControl<TextBox>(formInstance, "tFlangeTopRD_", i),
                    FlangeTopYYTextBox = GetControl<TextBox>(formInstance, "tFlangeTopYY_", i),
                    FlangeTopXTextBox = GetControl<TextBox>(formInstance, "tFlangeTopX_", i),
                    FlangeTopRTextBox = GetControl<TextBox>(formInstance, "tFlangeTopR_", i),
                    FlangeTopOTextBox = GetControl<TextBox>(formInstance, "tFlangeTopO_", i),
                    FlangeTopBCTextBox = GetControl<TextBox>(formInstance, "tFlangeTopBC_", i),
                    FlangeTopNBTextBox = GetControl<TextBox>(formInstance, "tFlangeTopNB_", i),
                    FlangeTopDBTextBox = GetControl<TextBox>(formInstance, "tFlangeTopDB_", i),
                    FlangeTopOffsetXTextBox = GetControl<TextBox>(formInstance, "tFlangeTopOffsetX_", i),
                    FlangeTopOffsetYTextBox = GetControl<TextBox>(formInstance, "tFlangeTopOffsetY_", i),
                    FlangeTopCountTextBox = GetControl<TextBox>(formInstance, "tFlangeTopCount_", i),
                    FlangeTopSpacingTextBox = GetControl<TextBox>(formInstance, "tFlangeTopSpacing_", i),
                    EndPlateBustedSpan2TextBox = GetControl<TextBox>(formInstance, "tEndPlateBustedSpan2_", i),
                };

                HeaderAppData.Add(i.ToString(), uiDto);
            }
        }


        // Map Prego data to DTO
        static Dictionary<string, Prego_DTO> _headerPregoData
        {
            get
            {
                return new Dictionary<string, Prego_DTO>
                {
                    ["61"] = new Prego_DTO
                    {
                        HeaderRequired = (new string[] { "AD36", "AD35" }, InputSheet),
                        BoxWidth = (new string[] { "AE42", "AD42" }, InputSheet),
                        TubesheetLength = (new string[] { "V23" }, InventorSheet),
                        TubesheetWidth = (new string[] { "V22" }, InventorSheet),
                        TopAndBottomPlateTHK = (new string[] { "AE51", "AD51" }, InputSheet),
                        BoxHeight = (new string[] { "CP180", }, SketchCalcsSheet),
                        BoxLength = (new string[] { "BQ44" }, InputSheet),
                        EndPlateTHK = (new string[] { "VN20" }, InputsCalcsSheet),
                        TopBtmTHK = (new string[] { "ADP13" }, InputsCalcsSheet),
                        PlugsheetWidth = (new string[] { "V25", "V22" }, InventorSheet),
                        PlugsheetLength = (new string[] { "V26", "V23" }, InventorSheet),
                        TopBtmLength = (new string[] { "V11" }, InventorSheet),
                        TopBtmWidth = (new string[] { "V9" }, InventorSheet),
                        EndPlateLength = (new string[] { "V16" }, InventorSheet),
                        EndPlateWidth = (new string[] { "V15" }, InventorSheet),
                        TubeHoleDiameter = (new string[] { "V39" }, InventorSheet),
                        TubeOddX = (new string[] { "V33" }, InventorSheet),
                        TubeEvenX = (new string[] { "V42" }, InventorSheet),
                        TubeY = (new string[] { "V32" }, InventorSheet),
                        TubeRow1Count = (new string[] { "V31" }, InventorSheet),
                        TubeRow2Count = (new string[] { "V40" }, InventorSheet),
                        TubeRow3Count = (new string[] { "V49" }, InventorSheet),
                        TubeRow4Count = (new string[] { "V58" }, InventorSheet),
                        TubeRow5Count = (new string[] { "V67" }, InventorSheet),
                        TubeRow6Count = (new string[] { "V76" }, InventorSheet),
                        TubeRow7Count = (new string[] { "V85" }, InventorSheet),
                        TubeRow8Count = (new string[] { "V94" }, InventorSheet),
                        TubeRow9Count = (new string[] { "V103" }, InventorSheet),
                        TubeRow10Count = (new string[] { "V112" }, InventorSheet),
                        TubeRow11Count = (new string[] { "V121" }, InventorSheet),
                        TubeRow12Count = (new string[] { "V130" }, InventorSheet),
                        TubeHPitchOdd = (new string[] { "V34" }, InventorSheet),
                        TubeHPitchEven = (new string[] { "V43" }, InventorSheet),
                        TubeVPitchOneTwo = (new string[] { "V41" }, InventorSheet),
                        TubeVPitchTwoThree = (new string[] { "V50" }, InventorSheet),
                        TubeVPitchThreeFour = (new string[] { "V59" }, InventorSheet),
                        TubeVPitchFourFive = (new string[] { "V68" }, InventorSheet),
                        TubeVPitchFiveSix = (new string[] { "V77" }, InventorSheet),
                        TubeVPitchSixSeven = (new string[] { "V86" }, InventorSheet),
                        TubeVPitchSevenEight = (new string[] { "V95" }, InventorSheet),
                        TubeVPitchEightNine = (new string[] { "V104" }, InventorSheet),
                        TubeVPitchNineTen = (new string[] { "V113" }, InventorSheet),
                        TubeVPitchTenEleven = (new string[] { "V122" }, InventorSheet),
                        TubeVPitchElevenTwelve = (new string[] { "V131" }, InventorSheet),
                        TubesheetTHK = (new string[] { "V21" }, InventorSheet),
                        PlugsheetTHK = (new string[] { "V24", "V21" }, InventorSheet),
                        StiffenerTHK = (new string[] { "V491" }, InventorSheet),
                        StiffenerWidth = (new string[] { "V492" }, InventorSheet),
                        StiffenerOffset = (new string[] { "V490" }, InventorSheet),
                        StiffenerBelowRow = (new string[] { "V488" }, InventorSheet),
                        StiffenerDistanceBelow = (new string[] { "V489" }, InventorSheet),
                        StiffenerWindowWidth = (new string[] { "V493" }, InventorSheet),
                        StiffenerWindowLength = (new string[] { "V494" }, InventorSheet),
                        StiffenerWindowQuantity = (new string[] { "V495" }, InventorSheet),
                        StiffenerWindowSpacing = (new string[] { "V496" }, InventorSheet),
                        StiffenerWindowOffset = (new string[] { "V497" }, InventorSheet),
                        PartitionTHK = (new string[] { "V645" }, InventorSheet),
                        PartitionWidth = (new string[] { "V646" }, InventorSheet),
                        PartitionBelowRow = (new string[] { "V641" }, InventorSheet),
                        PartitionDistanceBelow = (new string[] { "V647" }, InventorSheet),
                        FlangeTopCount = (new string[] { "AMZ17" }, InputsCalcsSheet),
                        FlangeTopOD = (new string[] { "CY40" }, InputsCalcsSheet),
                        FlangeTopID = (new string[] { "ASD25" }, InputsCalcsSheet),
                        FlangeTopQ = (new string[] { "AQX15" }, InputsCalcsSheet),
                        FlangeTopRD = (new string[] { "AQX18" }, InputsCalcsSheet),
                        FlangeTopYY = (new string[] { "AQX27" }, InputsCalcsSheet),
                        FlangeTopX = (new string[] { "AQX17" }, InputsCalcsSheet),
                        FlangeTopR = (new string[] { "AQX16" }, InputsCalcsSheet),
                        FlangeTopO = (new string[] { "AQX14" }, InputsCalcsSheet),
                        FlangeTopBC = (new string[] { "AQX26" }, InputsCalcsSheet),
                        FlangeTopNB = (new string[] { "AQX24" }, InputsCalcsSheet),
                        FlangeTopDB = (new string[] { "AQX25" }, InputsCalcsSheet),
                        FlangeTopOffsetX = (new string[] { "DO40" }, InputsCalcsSheet),
                        FlangeTopOffsetY = (new string[] { "DL60" }, InputsCalcsSheet),
                        FlangeTopSpacing = (new string[] { "DO37" }, InputsCalcsSheet),
                    },
                    ["63"] = new Prego_DTO
                    {
                        HeaderRequired = (new string[] { "AG36", "AG35" }, InputSheet),
                        BoxWidth = (new string[] { "AI42", "AG42" }, InputSheet),
                        TubesheetLength = (new string[] { "W23" }, InventorSheet),
                        TubesheetWidth = (new string[] { "W22" }, InventorSheet),
                        TopAndBottomPlateTHK = (new string[] { "AI51", "AG51" }, InputSheet),
                        BoxHeight = (new string[] { "CP181", }, SketchCalcsSheet),
                        BoxLength = (new string[] { "BQ44" }, InputSheet),
                        EndPlateTHK = (new string[] { "VO20" }, InputsCalcsSheet),
                        TopBtmTHK = (new string[] { "ADQ13" }, InputsCalcsSheet),
                        PlugsheetWidth = (new string[] { "W25", "W22" }, InventorSheet),
                        PlugsheetLength = (new string[] { "W26", "W23" }, InventorSheet),
                        TopBtmLength = (new string[] { "W11" }, InventorSheet),
                        TopBtmWidth = (new string[] { "W9" }, InventorSheet),
                        EndPlateLength = (new string[] { "W16" }, InventorSheet),
                        EndPlateWidth = (new string[] { "W15" }, InventorSheet),
                        TubeHoleDiameter = (new string[] { "W39" }, InventorSheet),
                        TubeOddX = (new string[] { "W33" }, InventorSheet),
                        TubeEvenX = (new string[] { "W42" }, InventorSheet),
                        TubeY = (new string[] { "W32" }, InventorSheet),
                        TubeRow1Count = (new string[] { "W31" }, InventorSheet),
                        TubeRow2Count = (new string[] { "W40" }, InventorSheet),
                        TubeRow3Count = (new string[] { "W49" }, InventorSheet),
                        TubeRow4Count = (new string[] { "W58" }, InventorSheet),
                        TubeRow5Count = (new string[] { "W67" }, InventorSheet),
                        TubeRow6Count = (new string[] { "W76" }, InventorSheet),
                        TubeRow7Count = (new string[] { "W85" }, InventorSheet),
                        TubeRow8Count = (new string[] { "W94" }, InventorSheet),
                        TubeRow9Count = (new string[] { "W103" }, InventorSheet),
                        TubeRow10Count = (new string[] { "W112" }, InventorSheet),
                        TubeRow11Count = (new string[] { "W121" }, InventorSheet),
                        TubeRow12Count = (new string[] { "W130" }, InventorSheet),
                        TubeHPitchOdd = (new string[] { "W34" }, InventorSheet),
                        TubeHPitchEven = (new string[] { "W43" }, InventorSheet),
                        TubeVPitchOneTwo = (new string[] { "W41" }, InventorSheet),
                        TubeVPitchTwoThree = (new string[] { "W50" }, InventorSheet),
                        TubeVPitchThreeFour = (new string[] { "W59" }, InventorSheet),
                        TubeVPitchFourFive = (new string[] { "W68" }, InventorSheet),
                        TubeVPitchFiveSix = (new string[] { "W77" }, InventorSheet),
                        TubeVPitchSixSeven = (new string[] { "W86" }, InventorSheet),
                        TubeVPitchSevenEight = (new string[] { "W95" }, InventorSheet),
                        TubeVPitchEightNine = (new string[] { "W104" }, InventorSheet),
                        TubeVPitchNineTen = (new string[] { "W113" }, InventorSheet),
                        TubeVPitchTenEleven = (new string[] { "W122" }, InventorSheet),
                        TubeVPitchElevenTwelve = (new string[] { "W131" }, InventorSheet),
                        TubesheetTHK = (new string[] { "W21" }, InventorSheet),
                        PlugsheetTHK = (new string[] { "W24", "W21" }, InventorSheet),
                        StiffenerTHK = (new string[] { "W491" }, InventorSheet),
                        StiffenerWidth = (new string[] { "W492" }, InventorSheet),
                        StiffenerOffset = (new string[] { "W490" }, InventorSheet),
                        StiffenerBelowRow = (new string[] { "W488" }, InventorSheet),
                        StiffenerDistanceBelow = (new string[] { "W489" }, InventorSheet),
                        StiffenerWindowWidth = (new string[] { "W493" }, InventorSheet),
                        StiffenerWindowLength = (new string[] { "W494" }, InventorSheet),
                        StiffenerWindowQuantity = (new string[] { "W495" }, InventorSheet),
                        StiffenerWindowSpacing = (new string[] { "W496" }, InventorSheet),
                        StiffenerWindowOffset = (new string[] { "W497" }, InventorSheet),
                        PartitionTHK = (new string[] { "W645" }, InventorSheet),
                        PartitionWidth = (new string[] { "W646" }, InventorSheet),
                        PartitionBelowRow = (new string[] { "W641" }, InventorSheet),
                        PartitionDistanceBelow = (new string[] { "W647" }, InventorSheet),
                        FlangeTopCount = (new string[] { "A67" }, InputSheet),
                        FlangeTopOD = (new string[] { "CY40" }, InputsCalcsSheet),
                        FlangeTopID = (new string[] { "ASD25" }, InputsCalcsSheet),
                        FlangeTopQ = (new string[] { "AQX15" }, InputsCalcsSheet),
                        FlangeTopRD = (new string[] { "AQX18" }, InputsCalcsSheet),
                        FlangeTopYY = (new string[] { "AQX27" }, InputsCalcsSheet),
                        FlangeTopX = (new string[] { "AQX17" }, InputsCalcsSheet),
                        FlangeTopR = (new string[] { "AQX16" }, InputsCalcsSheet),
                        FlangeTopO = (new string[] { "AQX14" }, InputsCalcsSheet),
                        FlangeTopBC = (new string[] { "AQX26" }, InputsCalcsSheet),
                        FlangeTopNB = (new string[] { "AQX24" }, InputsCalcsSheet),
                        FlangeTopDB = (new string[] { "AQX25" }, InputsCalcsSheet),
                        FlangeTopOffsetX = (new string[] { "DO40" }, InputsCalcsSheet),
                        FlangeTopOffsetY = (new string[] { "DL60" }, InputsCalcsSheet),
                        FlangeTopSpacing = (new string[] { "DO37" }, InputsCalcsSheet),
                    },
                    ["65"] = new Prego_DTO
                    {
                        HeaderRequired = (new string[] { "AJ36", "AJ35" }, InputSheet),
                        BoxWidth = (new string[] { "AK42", "AJ42" }, InputSheet),
                        TubesheetLength = (new string[] { "X23" }, InventorSheet),
                        TubesheetWidth = (new string[] { "X22" }, InventorSheet),
                        TopAndBottomPlateTHK = (new string[] { "AK51", "AJ51" }, InputSheet),
                        BoxHeight = (new string[] { "CP182", }, SketchCalcsSheet),
                        BoxLength = (new string[] { "BQ44" }, InputSheet),
                        EndPlateTHK = (new string[] { "VP20" }, InputsCalcsSheet),
                        TopBtmTHK = (new string[] { "ADR13" }, InputsCalcsSheet),
                        PlugsheetWidth = (new string[] { "X25", "X22" }, InventorSheet),
                        PlugsheetLength = (new string[] { "X26", "X23" }, InventorSheet),
                        TopBtmLength = (new string[] { "X11" }, InventorSheet),
                        TopBtmWidth = (new string[] { "X9" }, InventorSheet),
                        EndPlateLength = (new string[] { "X16" }, InventorSheet),
                        EndPlateWidth = (new string[] { "X15" }, InventorSheet),
                        TubeHoleDiameter = (new string[] { "X39" }, InventorSheet),
                        TubeOddX = (new string[] { "X33" }, InventorSheet),
                        TubeEvenX = (new string[] { "X42" }, InventorSheet),
                        TubeY = (new string[] { "X32" }, InventorSheet),
                        TubeRow1Count = (new string[] { "X31" }, InventorSheet),
                        TubeRow2Count = (new string[] { "X40" }, InventorSheet),
                        TubeRow3Count = (new string[] { "X49" }, InventorSheet),
                        TubeRow4Count = (new string[] { "X58" }, InventorSheet),
                        TubeRow5Count = (new string[] { "X67" }, InventorSheet),
                        TubeRow6Count = (new string[] { "X76" }, InventorSheet),
                        TubeRow7Count = (new string[] { "X85" }, InventorSheet),
                        TubeRow8Count = (new string[] { "X94" }, InventorSheet),
                        TubeRow9Count = (new string[] { "X103" }, InventorSheet),
                        TubeRow10Count = (new string[] { "X112" }, InventorSheet),
                        TubeRow11Count = (new string[] { "X121" }, InventorSheet),
                        TubeRow12Count = (new string[] { "X130" }, InventorSheet),
                        TubeHPitchOdd = (new string[] { "X34" }, InventorSheet),
                        TubeHPitchEven = (new string[] { "X43" }, InventorSheet),
                        TubeVPitchOneTwo = (new string[] { "X41" }, InventorSheet),
                        TubeVPitchTwoThree = (new string[] { "X50" }, InventorSheet),
                        TubeVPitchThreeFour = (new string[] { "X59" }, InventorSheet),
                        TubeVPitchFourFive = (new string[] { "X68" }, InventorSheet),
                        TubeVPitchFiveSix = (new string[] { "X77" }, InventorSheet),
                        TubeVPitchSixSeven = (new string[] { "X86" }, InventorSheet),
                        TubeVPitchSevenEight = (new string[] { "X95" }, InventorSheet),
                        TubeVPitchEightNine = (new string[] { "X104" }, InventorSheet),
                        TubeVPitchNineTen = (new string[] { "X113" }, InventorSheet),
                        TubeVPitchTenEleven = (new string[] { "X122" }, InventorSheet),
                        TubeVPitchElevenTwelve = (new string[] { "X131" }, InventorSheet),
                        TubesheetTHK = (new string[] { "X21" }, InventorSheet),
                        PlugsheetTHK = (new string[] { "X24", "X21" }, InventorSheet),
                        StiffenerTHK = (new string[] { "X491" }, InventorSheet),
                        StiffenerWidth = (new string[] { "X492" }, InventorSheet),
                        StiffenerOffset = (new string[] { "X490" }, InventorSheet),
                        StiffenerBelowRow = (new string[] { "X488" }, InventorSheet),
                        StiffenerDistanceBelow = (new string[] { "X489" }, InventorSheet),
                        StiffenerWindowWidth = (new string[] { "X493" }, InventorSheet),
                        StiffenerWindowLength = (new string[] { "X494" }, InventorSheet),
                        StiffenerWindowQuantity = (new string[] { "X495" }, InventorSheet),
                        StiffenerWindowSpacing = (new string[] { "X496" }, InventorSheet),
                        StiffenerWindowOffset = (new string[] { "X497" }, InventorSheet),
                        PartitionTHK = (new string[] { "X645" }, InventorSheet),
                        PartitionWidth = (new string[] { "X646" }, InventorSheet),
                        PartitionBelowRow = (new string[] { "X641" }, InventorSheet),
                        PartitionDistanceBelow = (new string[] { "X647" }, InventorSheet),
                        FlangeTopCount = (new string[] { "A67" }, InputSheet),
                        FlangeTopOD = (new string[] { "CY40" }, InputsCalcsSheet),
                        FlangeTopID = (new string[] { "ASD25" }, InputsCalcsSheet),
                        FlangeTopQ = (new string[] { "AQX15" }, InputsCalcsSheet),
                        FlangeTopRD = (new string[] { "AQX18" }, InputsCalcsSheet),
                        FlangeTopYY = (new string[] { "AQX27" }, InputsCalcsSheet),
                        FlangeTopX = (new string[] { "AQX17" }, InputsCalcsSheet),
                        FlangeTopR = (new string[] { "AQX16" }, InputsCalcsSheet),
                        FlangeTopO = (new string[] { "AQX14" }, InputsCalcsSheet),
                        FlangeTopBC = (new string[] { "AQX26" }, InputsCalcsSheet),
                        FlangeTopNB = (new string[] { "AQX24" }, InputsCalcsSheet),
                        FlangeTopDB = (new string[] { "AQX25" }, InputsCalcsSheet),
                        FlangeTopOffsetX = (new string[] { "DO40" }, InputsCalcsSheet),
                        FlangeTopOffsetY = (new string[] { "DL60" }, InputsCalcsSheet),
                        FlangeTopSpacing = (new string[] { "DO37" }, InputsCalcsSheet),
                    },
                    ["62"] = new Prego_DTO
                    {
                        HeaderRequired = (new string[] { "AL36", "AL35" }, InputSheet),
                        BoxWidth = (new string[] { "AM42", "AL42" }, InputSheet),
                        TubesheetLength = (new string[] { "Y23" }, InventorSheet),
                        TubesheetWidth = (new string[] { "Y22" }, InventorSheet),
                        TopAndBottomPlateTHK = (new string[] { "AM51", "AL51" }, InputSheet),
                        BoxHeight = (new string[] { "CP183", }, SketchCalcsSheet),
                        BoxLength = (new string[] { "BQ44" }, InputSheet),
                        EndPlateTHK = (new string[] { "VQ20" }, InputsCalcsSheet),
                        TopBtmTHK = (new string[] { "ADS13" }, InputsCalcsSheet),
                        PlugsheetWidth = (new string[] { "Y25", "Y22" }, InventorSheet),
                        PlugsheetLength = (new string[] { "Y26", "Y23" }, InventorSheet),
                        TopBtmLength = (new string[] { "Y11" }, InventorSheet),
                        TopBtmWidth = (new string[] { "Y9" }, InventorSheet),
                        EndPlateLength = (new string[] { "Y16" }, InventorSheet),
                        EndPlateWidth = (new string[] { "Y15" }, InventorSheet),
                        TubeHoleDiameter = (new string[] { "Y39" }, InventorSheet),
                        TubeOddX = (new string[] { "Y33" }, InventorSheet),
                        TubeEvenX = (new string[] { "Y42" }, InventorSheet),
                        TubeY = (new string[] { "Y32" }, InventorSheet),
                        TubeRow1Count = (new string[] { "Y31" }, InventorSheet),
                        TubeRow2Count = (new string[] { "Y40" }, InventorSheet),
                        TubeRow3Count = (new string[] { "Y49" }, InventorSheet),
                        TubeRow4Count = (new string[] { "Y58" }, InventorSheet),
                        TubeRow5Count = (new string[] { "Y67" }, InventorSheet),
                        TubeRow6Count = (new string[] { "Y76" }, InventorSheet),
                        TubeRow7Count = (new string[] { "Y85" }, InventorSheet),
                        TubeRow8Count = (new string[] { "Y94" }, InventorSheet),
                        TubeRow9Count = (new string[] { "Y103" }, InventorSheet),
                        TubeRow10Count = (new string[] { "Y112" }, InventorSheet),
                        TubeRow11Count = (new string[] { "Y121" }, InventorSheet),
                        TubeRow12Count = (new string[] { "Y130" }, InventorSheet),
                        TubeHPitchOdd = (new string[] { "Y34" }, InventorSheet),
                        TubeHPitchEven = (new string[] { "Y43" }, InventorSheet),
                        TubeVPitchOneTwo = (new string[] { "Y41" }, InventorSheet),
                        TubeVPitchTwoThree = (new string[] { "Y50" }, InventorSheet),
                        TubeVPitchThreeFour = (new string[] { "Y59" }, InventorSheet),
                        TubeVPitchFourFive = (new string[] { "Y68" }, InventorSheet),
                        TubeVPitchFiveSix = (new string[] { "Y77" }, InventorSheet),
                        TubeVPitchSixSeven = (new string[] { "Y86" }, InventorSheet),
                        TubeVPitchSevenEight = (new string[] { "Y95" }, InventorSheet),
                        TubeVPitchEightNine = (new string[] { "Y104" }, InventorSheet),
                        TubeVPitchNineTen = (new string[] { "Y113" }, InventorSheet),
                        TubeVPitchTenEleven = (new string[] { "Y122" }, InventorSheet),
                        TubeVPitchElevenTwelve = (new string[] { "Y131" }, InventorSheet),
                        TubesheetTHK = (new string[] { "Y21" }, InventorSheet),
                        PlugsheetTHK = (new string[] { "Y24", "Y21" }, InventorSheet),
                        StiffenerTHK = (new string[] { "Y491" }, InventorSheet),
                        StiffenerWidth = (new string[] { "Y492" }, InventorSheet),
                        StiffenerOffset = (new string[] { "Y490" }, InventorSheet),
                        StiffenerBelowRow = (new string[] { "Y488" }, InventorSheet),
                        StiffenerDistanceBelow = (new string[] { "Y489" }, InventorSheet),
                        StiffenerWindowWidth = (new string[] { "Y493" }, InventorSheet),
                        StiffenerWindowLength = (new string[] { "Y494" }, InventorSheet),
                        StiffenerWindowQuantity = (new string[] { "Y495" }, InventorSheet),
                        StiffenerWindowSpacing = (new string[] { "Y496" }, InventorSheet),
                        StiffenerWindowOffset = (new string[] { "Y497" }, InventorSheet),
                        PartitionTHK = (new string[] { "Y645" }, InventorSheet),
                        PartitionWidth = (new string[] { "Y646" }, InventorSheet),
                        PartitionBelowRow = (new string[] { "Y641" }, InventorSheet),
                        PartitionDistanceBelow = (new string[] { "Y647" }, InventorSheet),
                        FlangeTopCount = (new string[] { "AMZ73" }, InputsCalcsSheet),
                        FlangeTopOD = (new string[] { "CY40" }, InputsCalcsSheet),
                        FlangeTopID = (new string[] { "ASD25" }, InputsCalcsSheet),
                        FlangeTopQ = (new string[] { "AQX15" }, InputsCalcsSheet),
                        FlangeTopRD = (new string[] { "AQX18" }, InputsCalcsSheet),
                        FlangeTopYY = (new string[] { "AQX27" }, InputsCalcsSheet),
                        FlangeTopX = (new string[] { "AQX17" }, InputsCalcsSheet),
                        FlangeTopR = (new string[] { "AQX16" }, InputsCalcsSheet),
                        FlangeTopO = (new string[] { "AQX14" }, InputsCalcsSheet),
                        FlangeTopBC = (new string[] { "AQX26" }, InputsCalcsSheet),
                        FlangeTopNB = (new string[] { "AQX24" }, InputsCalcsSheet),
                        FlangeTopDB = (new string[] { "AQX25" }, InputsCalcsSheet),
                        FlangeTopOffsetX = (new string[] { "DO40" }, InputsCalcsSheet),
                        FlangeTopOffsetY = (new string[] { "DL60" }, InputsCalcsSheet),
                        FlangeTopSpacing = (new string[] { "DO37" }, InputsCalcsSheet),

                    },

                    ["64"] = new Prego_DTO
                    {
                        HeaderRequired = (new string[] { "AO36", "AO35" }, InputSheet),
                        BoxWidth = (new string[] { "AQ42", "AO42" }, InputSheet),
                        TubesheetLength = (new string[] { "Z23" }, InventorSheet),
                        TubesheetWidth = (new string[] { "Z22" }, InventorSheet),
                        TopAndBottomPlateTHK = (new string[] { "AQ51", "AO51" }, InputSheet),
                        BoxHeight = (new string[] { "CP184", }, SketchCalcsSheet),
                        BoxLength = (new string[] { "BQ44" }, InputSheet),
                        EndPlateTHK = (new string[] { "VR20" }, InputsCalcsSheet),
                        TopBtmTHK = (new string[] { "ADT13" }, InputsCalcsSheet),
                        PlugsheetWidth = (new string[] { "Z25", "Z22" }, InventorSheet),
                        PlugsheetLength = (new string[] { "Z26", "Z23" }, InventorSheet),
                        TopBtmLength = (new string[] { "Z11" }, InventorSheet),
                        TopBtmWidth = (new string[] { "Z9" }, InventorSheet),
                        EndPlateLength = (new string[] { "Z16" }, InventorSheet),
                        EndPlateWidth = (new string[] { "Z15" }, InventorSheet),
                        TubeHoleDiameter = (new string[] { "Z39" }, InventorSheet),
                        TubeOddX = (new string[] { "Z33" }, InventorSheet),
                        TubeEvenX = (new string[] { "Z42" }, InventorSheet),
                        TubeY = (new string[] { "Z32" }, InventorSheet),
                        TubeRow1Count = (new string[] { "Z31" }, InventorSheet),
                        TubeRow2Count = (new string[] { "Z40" }, InventorSheet),
                        TubeRow3Count = (new string[] { "Z49" }, InventorSheet),
                        TubeRow4Count = (new string[] { "Z58" }, InventorSheet),
                        TubeRow5Count = (new string[] { "Z67" }, InventorSheet),
                        TubeRow6Count = (new string[] { "Z76" }, InventorSheet),
                        TubeRow7Count = (new string[] { "Z85" }, InventorSheet),
                        TubeRow8Count = (new string[] { "Z94" }, InventorSheet),
                        TubeRow9Count = (new string[] { "Z103" }, InventorSheet),
                        TubeRow10Count = (new string[] { "Z112" }, InventorSheet),
                        TubeRow11Count = (new string[] { "Z121" }, InventorSheet),
                        TubeRow12Count = (new string[] { "Z130" }, InventorSheet),
                        TubeHPitchOdd = (new string[] { "Z34" }, InventorSheet),
                        TubeHPitchEven = (new string[] { "Z43" }, InventorSheet),
                        TubeVPitchOneTwo = (new string[] { "Z41" }, InventorSheet),
                        TubeVPitchTwoThree = (new string[] { "Z50" }, InventorSheet),
                        TubeVPitchThreeFour = (new string[] { "Z59" }, InventorSheet),
                        TubeVPitchFourFive = (new string[] { "Z68" }, InventorSheet),
                        TubeVPitchFiveSix = (new string[] { "Z77" }, InventorSheet),
                        TubeVPitchSixSeven = (new string[] { "Z86" }, InventorSheet),
                        TubeVPitchSevenEight = (new string[] { "Z95" }, InventorSheet),
                        TubeVPitchEightNine = (new string[] { "Z104" }, InventorSheet),
                        TubeVPitchNineTen = (new string[] { "Z113" }, InventorSheet),
                        TubeVPitchTenEleven = (new string[] { "Z122" }, InventorSheet),
                        TubeVPitchElevenTwelve = (new string[] { "Z131" }, InventorSheet),
                        TubesheetTHK = (new string[] { "Z21" }, InventorSheet),
                        PlugsheetTHK = (new string[] { "Z24", "Z21" }, InventorSheet),
                        StiffenerTHK = (new string[] { "Z491" }, InventorSheet),
                        StiffenerWidth = (new string[] { "Z492" }, InventorSheet),
                        StiffenerOffset = (new string[] { "Z490" }, InventorSheet),
                        StiffenerBelowRow = (new string[] { "Z488" }, InventorSheet),
                        StiffenerDistanceBelow = (new string[] { "V489" }, InventorSheet),
                        StiffenerWindowWidth = (new string[] { "Z493" }, InventorSheet),
                        StiffenerWindowLength = (new string[] { "Z494" }, InventorSheet),
                        StiffenerWindowQuantity = (new string[] { "Z495" }, InventorSheet),
                        StiffenerWindowSpacing = (new string[] { "Z496" }, InventorSheet),
                        StiffenerWindowOffset = (new string[] { "Z497" }, InventorSheet),
                        PartitionTHK = (new string[] { "Z645" }, InventorSheet),
                        PartitionWidth = (new string[] { "Z646" }, InventorSheet),
                        PartitionBelowRow = (new string[] { "Z641" }, InventorSheet),
                        PartitionDistanceBelow = (new string[] { "Z647" }, InventorSheet),
                        FlangeTopCount = (new string[] { "A67" }, InputSheet),
                        FlangeTopOD = (new string[] { "CY40" }, InputsCalcsSheet),
                        FlangeTopID = (new string[] { "ASD25" }, InputsCalcsSheet),
                        FlangeTopQ = (new string[] { "AQX15" }, InputsCalcsSheet),
                        FlangeTopRD = (new string[] { "AQX18" }, InputsCalcsSheet),
                        FlangeTopYY = (new string[] { "AQX27" }, InputsCalcsSheet),
                        FlangeTopX = (new string[] { "AQX17" }, InputsCalcsSheet),
                        FlangeTopR = (new string[] { "AQX16" }, InputsCalcsSheet),
                        FlangeTopO = (new string[] { "AQX14" }, InputsCalcsSheet),
                        FlangeTopBC = (new string[] { "AQX26" }, InputsCalcsSheet),
                        FlangeTopNB = (new string[] { "AQX24" }, InputsCalcsSheet),
                        FlangeTopDB = (new string[] { "AQX25" }, InputsCalcsSheet),
                        FlangeTopOffsetX = (new string[] { "DO40" }, InputsCalcsSheet),
                        FlangeTopOffsetY = (new string[] { "DL60" }, InputsCalcsSheet),
                        FlangeTopSpacing = (new string[] { "DO37" }, InputsCalcsSheet),
                    },
                    ["66"] = new Prego_DTO
                    {
                        HeaderRequired = (new string[] { "AR36", "AR35" }, InputSheet),
                        BoxWidth = (new string[] { "AS42", "AR42" }, InputSheet),
                        TubesheetLength = (new string[] { "AA23" }, InventorSheet),
                        TubesheetWidth = (new string[] { "AA22" }, InventorSheet),
                        TopAndBottomPlateTHK = (new string[] { "AS51", "AR51" }, InputSheet),
                        BoxHeight = (new string[] { "CP185", }, SketchCalcsSheet),
                        BoxLength = (new string[] { "BQ44" }, InputSheet),
                        EndPlateTHK = (new string[] { "VS20" }, InputsCalcsSheet),
                        TopBtmTHK = (new string[] { "ADU13" }, InputsCalcsSheet),
                        PlugsheetWidth = (new string[] { "AA25", "AA22" }, InventorSheet),
                        PlugsheetLength = (new string[] { "AA26", "AA23" }, InventorSheet),
                        TopBtmLength = (new string[] { "AA11" }, InventorSheet),
                        TopBtmWidth = (new string[] { "AA9" }, InventorSheet),
                        EndPlateLength = (new string[] { "AA16" }, InventorSheet),
                        EndPlateWidth = (new string[] { "AA15" }, InventorSheet),
                        TubeHoleDiameter = (new string[] { "AA39" }, InventorSheet),
                        TubeOddX = (new string[] { "AA33" }, InventorSheet),
                        TubeEvenX = (new string[] { "AA42" }, InventorSheet),
                        TubeY = (new string[] { "AA32" }, InventorSheet),
                        TubeRow1Count = (new string[] { "AA31" }, InventorSheet),
                        TubeRow2Count = (new string[] { "AA40" }, InventorSheet),
                        TubeRow3Count = (new string[] { "AA49" }, InventorSheet),
                        TubeRow4Count = (new string[] { "AA58" }, InventorSheet),
                        TubeRow5Count = (new string[] { "AA67" }, InventorSheet),
                        TubeRow6Count = (new string[] { "AA76" }, InventorSheet),
                        TubeRow7Count = (new string[] { "AA85" }, InventorSheet),
                        TubeRow8Count = (new string[] { "AA94" }, InventorSheet),
                        TubeRow9Count = (new string[] { "AA103" }, InventorSheet),
                        TubeRow10Count = (new string[] { "AA112" }, InventorSheet),
                        TubeRow11Count = (new string[] { "AA121" }, InventorSheet),
                        TubeRow12Count = (new string[] { "AA130" }, InventorSheet),
                        TubeHPitchOdd = (new string[] { "AA34" }, InventorSheet),
                        TubeHPitchEven = (new string[] { "AA43" }, InventorSheet),
                        TubeVPitchOneTwo = (new string[] { "AA41" }, InventorSheet),
                        TubeVPitchTwoThree = (new string[] { "AA50" }, InventorSheet),
                        TubeVPitchThreeFour = (new string[] { "AA59" }, InventorSheet),
                        TubeVPitchFourFive = (new string[] { "AA68" }, InventorSheet),
                        TubeVPitchFiveSix = (new string[] { "AA77" }, InventorSheet),
                        TubeVPitchSixSeven = (new string[] { "AA86" }, InventorSheet),
                        TubeVPitchSevenEight = (new string[] { "AA95" }, InventorSheet),
                        TubeVPitchEightNine = (new string[] { "AA104" }, InventorSheet),
                        TubeVPitchNineTen = (new string[] { "AA113" }, InventorSheet),
                        TubeVPitchTenEleven = (new string[] { "AA122" }, InventorSheet),
                        TubeVPitchElevenTwelve = (new string[] { "AA131" }, InventorSheet),
                        TubesheetTHK = (new string[] { "AA21" }, InventorSheet),
                        PlugsheetTHK = (new string[] { "AA24", "A21" }, InventorSheet),
                        StiffenerTHK = (new string[] { "AA491" }, InventorSheet),
                        StiffenerWidth = (new string[] { "AA492" }, InventorSheet),
                        StiffenerOffset = (new string[] { "AA490" }, InventorSheet),
                        StiffenerBelowRow = (new string[] { "AA488" }, InventorSheet),
                        StiffenerDistanceBelow = (new string[] { "AA489" }, InventorSheet),
                        StiffenerWindowWidth = (new string[] { "AA493" }, InventorSheet),
                        StiffenerWindowLength = (new string[] { "AA494" }, InventorSheet),
                        StiffenerWindowQuantity = (new string[] { "AA495" }, InventorSheet),
                        StiffenerWindowSpacing = (new string[] { "AA496" }, InventorSheet),
                        StiffenerWindowOffset = (new string[] { "AA497" }, InventorSheet),
                        PartitionTHK = (new string[] { "AA645" }, InventorSheet),
                        PartitionWidth = (new string[] { "AA646" }, InventorSheet),
                        PartitionBelowRow = (new string[] { "AA641" }, InventorSheet),
                        PartitionDistanceBelow = (new string[] { "AA647" }, InventorSheet),
                        FlangeTopCount = (new string[] { "A67" }, InputSheet),
                        FlangeTopOD = (new string[] { "CY40" }, InputsCalcsSheet),
                        FlangeTopID = (new string[] { "ASD25" }, InputsCalcsSheet),
                        FlangeTopQ = (new string[] { "AQX15" }, InputsCalcsSheet),
                        FlangeTopRD = (new string[] { "AQX18" }, InputsCalcsSheet),
                        FlangeTopYY = (new string[] { "AQX27" }, InputsCalcsSheet),
                        FlangeTopX = (new string[] { "AQX17" }, InputsCalcsSheet),
                        FlangeTopR = (new string[] { "AQX16" }, InputsCalcsSheet),
                        FlangeTopO = (new string[] { "AQX14" }, InputsCalcsSheet),
                        FlangeTopBC = (new string[] { "AQX26" }, InputsCalcsSheet),
                        FlangeTopNB = (new string[] { "AQX24" }, InputsCalcsSheet),
                        FlangeTopDB = (new string[] { "AQX25" }, InputsCalcsSheet),
                        FlangeTopOffsetX = (new string[] { "DO40" }, InputsCalcsSheet),
                        FlangeTopOffsetY = (new string[] { "DL60" }, InputsCalcsSheet),
                        FlangeTopSpacing = (new string[] { "DO37" }, InputsCalcsSheet),
                    }
                };
            }
        }

        #region Data loaders

        // Push application data to UI
        public static void LoadHeaderData_FromApp(string headerNumber)
        {
            var headerControls = HeaderAppData[headerNumber];
            headerControls.Enabled.Checked = headerControls.Header.IsRequired;

            ToggleTextbox_OnOff(headerControls);

            if (headerControls.Header.IsRequired)
                PushApplicationDataToTextbox(headerControls);
            else
                SetTextboxToEmptyString(headerControls);
        }


        // Push Prego data to application
        public static void LoadHeaderData_FromPrego(IHeaderExtensions header, CheckBox checkBox, UI_DTO uiDto, Prego_DTO prego)
        {
            header.IsRequired = LoadPregoBool_NullOrEmpty(checkBox, prego.HeaderRequired.Sheet, prego.HeaderRequired.Cells);
            if (header.IsRequired)
            {
                var uiDtoType = typeof(UI_DTO);
                var pregoDtoType = typeof(Prego_DTO);
                foreach (var uiDtoProperty in uiDtoType.GetProperties())
                {
                    if (uiDtoProperty.PropertyType == typeof(TextBox))
                    {
                        string propertyName = uiDtoProperty.Name.Replace("TextBox", "");
                        var pregoDtoProperty = pregoDtoType.GetProperty(propertyName);
                        if (pregoDtoProperty != null)
                        {
                            var textBox = (TextBox)uiDtoProperty.GetValue(uiDto);
                            var cellsAndSheet = (ValueTuple<string[], Worksheet>)pregoDtoProperty.GetValue(prego);
                            if (textBox != null && cellsAndSheet.Item1 != null && cellsAndSheet.Item2 != null)
                            {
                                Type headerType = header.GetType();
                                PropertyInfo headerProperty = headerType.GetProperty(propertyName);
                                double loadedValue = LoadPregoDouble(textBox, cellsAndSheet.Item2, cellsAndSheet.Item1);
                                textBox.Text = loadedValue == 0 ? "" : loadedValue.ToString();
                                headerProperty.SetValue(header, loadedValue);
                            }
                        }
                    }
                }
            }
        }


        // Push application data to UI
        public static void ImportHeaderData_FromPrego()
        {
            var headerNumbers = new[] { "61", "62", "63", "64", "65", "66" };
            foreach (var headerNumber in headerNumbers)
            {
                var headerAppData = HeaderAppData[headerNumber];
                var headerPregoData = _headerPregoData[headerNumber];
                LoadHeaderData_FromPrego
                (
                    headerAppData.Header,
                    headerAppData.Enabled,
                    headerAppData,
                    headerPregoData
                );
            }
            SaveSettings();

        }

        #endregion  
    }
}
