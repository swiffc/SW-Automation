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
            public (string[] Cells, Worksheet Sheet) TubeHPitchOdd { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeHPitchEven { get; set; }
            public (string[] Cells, Worksheet Sheet) TubeVPitchOneTwo { get; set; }
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
            public TextBox TubeHPitchOddTextBox { get; set; }
            public TextBox TubeHPitchEvenTextBox { get; set; }
            public TextBox TubeVPitchOneTwoTextBox { get; set; }
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
                    TubeHPitchOddTextBox = GetControl<TextBox>(formInstance, "tTubeHPitchOdd_", i),
                    TubeHPitchEvenTextBox = GetControl<TextBox>(formInstance, "tTubeHPitchEven_", i),
                    TubeVPitchOneTwoTextBox = GetControl<TextBox>(formInstance, "tTubeVPitchOneTwo_", i),
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
                        TubesheetTHK = (new string[] { "AE49", "AD49" }, InputSheet),
                        TubesheetLength = (new string[] { "V23" }, InventorSheet),
                        TubesheetWidth = (new string[] { "V22" }, InventorSheet),
                        PlugsheetTHK = (new string[] { "AE50", "AD50" }, InputSheet),
                        TopAndBottomPlateTHK = (new string[] { "AE51", "AD51" }, InputSheet),
                        BoxHeight = (new string[] { "CP180", }, SketchCalcsSheet),
                        BoxLength = (new string[] { "BQ44" }, InputSheet),
                        EndPlateTHK = (new string[] { "VN20" }, InputsCalcsSheet),
                        TopBtmTHK = (new string[] { "ADP13" }, InputsCalcsSheet),
                        PlugsheetWidth = (new string[] { "V25" }, InventorSheet),
                        PlugsheetLength = (new string[] { "V26" }, InventorSheet),
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
                        TubeHPitchOdd = (new string[] { "V34" }, InventorSheet),
                        TubeHPitchEven = (new string[] { "V43" }, InventorSheet),
                        TubeVPitchOneTwo = (new string[] { "V41" }, InventorSheet),
                    },
                    ["63"] = new Prego_DTO
                    {
                        HeaderRequired = (new string[] { "AG36", "AG35" }, InputSheet),
                        BoxWidth = (new string[] { "AI42", "AG42" }, InputSheet),
                        TubesheetTHK = (new string[] { "AI49", "AG49" }, InputSheet),
                        TubesheetLength = (new string[] { "W23" }, InventorSheet),
                        TubesheetWidth = (new string[] { "W22" }, InventorSheet),
                        PlugsheetTHK = (new string[] { "AI50", "AG50" }, InputSheet),
                        TopAndBottomPlateTHK = (new string[] { "AI51", "AG51" }, InputSheet),
                        BoxHeight = (new string[] { "CP181", }, SketchCalcsSheet),
                        BoxLength = (new string[] { "BQ44" }, InputSheet),
                        EndPlateTHK = (new string[] { "VO20" }, InputsCalcsSheet),
                        TopBtmTHK = (new string[] { "ADQ13" }, InputsCalcsSheet),
                        PlugsheetWidth = (new string[] { "W25" }, InventorSheet),
                        PlugsheetLength = (new string[] { "W26" }, InventorSheet),
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
                        TubeHPitchOdd = (new string[] { "W34" }, InventorSheet),
                        TubeHPitchEven = (new string[] { "W43" }, InventorSheet),
                        TubeVPitchOneTwo = (new string[] { "W41" }, InventorSheet),
                    },
                    ["65"] = new Prego_DTO
                    {
                        HeaderRequired = (new string[] { "AJ36", "AJ35" }, InputSheet),
                        BoxWidth = (new string[] { "AK42", "AJ42" }, InputSheet),
                        TubesheetTHK = (new string[] { "AK49", "AJ49" }, InputSheet),
                        TubesheetLength = (new string[] { "X23" }, InventorSheet),
                        TubesheetWidth = (new string[] { "X22" }, InventorSheet),
                        PlugsheetTHK = (new string[] { "AK50", "AJ50" }, InputSheet),
                        TopAndBottomPlateTHK = (new string[] { "AK51", "AJ51" }, InputSheet),
                        BoxHeight = (new string[] { "CP182", }, SketchCalcsSheet),
                        BoxLength = (new string[] { "BQ44" }, InputSheet),
                        EndPlateTHK = (new string[] { "VP20" }, InputsCalcsSheet),
                        TopBtmTHK = (new string[] { "ADR13" }, InputsCalcsSheet),
                        PlugsheetWidth = (new string[] { "X25" }, InventorSheet),
                        PlugsheetLength = (new string[] { "X26" }, InventorSheet),
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
                        TubeHPitchOdd = (new string[] { "X34" }, InventorSheet),
                        TubeHPitchEven = (new string[] { "X43" }, InventorSheet),
                        TubeVPitchOneTwo = (new string[] { "X41" }, InventorSheet),
                    },
                    ["62"] = new Prego_DTO
                    {
                        HeaderRequired = (new string[] { "AL36", "AL35" }, InputSheet),
                        BoxWidth = (new string[] { "AM42", "AL42" }, InputSheet),
                        TubesheetTHK = (new string[] { "AM49", "AL49" }, InputSheet),
                        TubesheetLength = (new string[] { "Y23" }, InventorSheet),
                        TubesheetWidth = (new string[] { "Y22" }, InventorSheet),
                        PlugsheetTHK = (new string[] { "AM50", "AL50" }, InputSheet),
                        TopAndBottomPlateTHK = (new string[] { "AM51", "AL51" }, InputSheet),
                        BoxHeight = (new string[] { "CP183", }, SketchCalcsSheet),
                        BoxLength = (new string[] { "BQ44" }, InputSheet),
                        EndPlateTHK = (new string[] { "VQ20" }, InputsCalcsSheet),
                        TopBtmTHK = (new string[] { "ADS13" }, InputsCalcsSheet),
                        PlugsheetWidth = (new string[] { "Y25" }, InventorSheet),
                        PlugsheetLength = (new string[] { "Y26" }, InventorSheet),
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
                        TubeHPitchOdd = (new string[] { "Y34" }, InventorSheet),
                        TubeHPitchEven = (new string[] { "Y43" }, InventorSheet),
                        TubeVPitchOneTwo = (new string[] { "Y41" }, InventorSheet),
                    },

                    ["64"] = new Prego_DTO
                    {
                        HeaderRequired = (new string[] { "AO36", "AO35" }, InputSheet),
                        BoxWidth = (new string[] { "AQ42", "AO42" }, InputSheet),
                        TubesheetTHK = (new string[] { "AQ49", "AO49" }, InputSheet),
                        TubesheetLength = (new string[] { "Z23" }, InventorSheet),
                        TubesheetWidth = (new string[] { "Z22" }, InventorSheet),
                        PlugsheetTHK = (new string[] { "AQ50", "AO50" }, InputSheet),
                        TopAndBottomPlateTHK = (new string[] { "AQ51", "AO51" }, InputSheet),
                        BoxHeight = (new string[] { "CP184", }, SketchCalcsSheet),
                        BoxLength = (new string[] { "BQ44" }, InputSheet),
                        EndPlateTHK = (new string[] { "VR20" }, InputsCalcsSheet),
                        TopBtmTHK = (new string[] { "ADT13" }, InputsCalcsSheet),
                        PlugsheetWidth = (new string[] { "Z25" }, InventorSheet),
                        PlugsheetLength = (new string[] { "Z26" }, InventorSheet),
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
                        TubeHPitchOdd = (new string[] { "Z34" }, InventorSheet),
                        TubeHPitchEven = (new string[] { "Z43" }, InventorSheet),
                        TubeVPitchOneTwo = (new string[] { "Z41" }, InventorSheet),
                    },
                    ["66"] = new Prego_DTO
                    {
                        HeaderRequired = (new string[] { "AR36", "AR35" }, InputSheet),
                        BoxWidth = (new string[] { "AS42", "AR42" }, InputSheet),
                        TubesheetTHK = (new string[] { "AS49", "AR49" }, InputSheet),
                        TubesheetLength = (new string[] { "AA23" }, InventorSheet),
                        TubesheetWidth = (new string[] { "AA22" }, InventorSheet),
                        PlugsheetTHK = (new string[] { "AS50", "AR50" }, InputSheet),
                        TopAndBottomPlateTHK = (new string[] { "AS51", "AR51" }, InputSheet),
                        BoxHeight = (new string[] { "CP185", }, SketchCalcsSheet),
                        BoxLength = (new string[] { "BQ44" }, InputSheet),
                        EndPlateTHK = (new string[] { "VS20" }, InputsCalcsSheet),
                        TopBtmTHK = (new string[] { "ADU13" }, InputsCalcsSheet),
                        PlugsheetWidth = (new string[] { "AA25" }, InventorSheet),
                        PlugsheetLength = (new string[] { "AA26" }, InventorSheet),
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
                        TubeHPitchOdd = (new string[] { "AA34" }, InventorSheet),
                        TubeHPitchEven = (new string[] { "AA43" }, InventorSheet),
                        TubeVPitchOneTwo = (new string[] { "AA41" }, InventorSheet),
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
            if (PregoDoc != null)
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
                MessageBox.Show($"Data imported from Prego successfully", "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
            else
            {
                MessageBox.Show("Prego file not found", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        #endregion  
    }
}
