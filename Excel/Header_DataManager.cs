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
            public (string[] Cells, Worksheet Sheet) PlugsheetTHK { get; set; }
            public (string[] Cells, Worksheet Sheet) TopAndBottomPlateTHK { get; set; }
            public (string[] Cells, Worksheet Sheet) VerticalSpan { get; set; }
            public (string[] Cells, Worksheet Sheet) BoxLength { get; set; }
            public (string[] Cells, Worksheet Sheet) Y_Location { get; set; }
            public (string[] Cells, Worksheet Sheet) Xtop { get; set; }
        }

        public static Dictionary<string, UI_DTO> HeaderAppData;
        public class UI_DTO
        {
            public IHeaderExtensions Header { get; set; }
            public CheckBox Enabled { get; set; }
            public TextBox BoxWidthTextBox { get; set; }
            public TextBox TubesheetTHKTextBox { get; set; }
            public TextBox PlugsheetTHKTextBox { get; set; }
            public TextBox TopAndBottomPlateTHKTextBox { get; set; }
            public TextBox BoxLengthTextBox { get; set; }
            public TextBox VerticalSpanTextBox { get; set; }
            public TextBox Y_LocationTextBox { get; set; }
            public TextBox XtopTextBox { get; set; }
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
                        PlugsheetTHK = (new string[] { "AE50", "AD50" }, InputSheet),
                        TopAndBottomPlateTHK = (new string[] { "AE51", "AD51" }, InputSheet),
                        VerticalSpan = (new string[] { "CP180", }, SketchCalcsSheet),
                        BoxLength = (new string[] { "BQ44" }, InputSheet),
                        Y_Location = (new string[] { "BGM37" }, InputsCalcsSheet),
                        Xtop = (new string[] { "AE47", "AD47" }, InputSheet),
                    },
                    ["62"] = new Prego_DTO
                    {
                        HeaderRequired = (new string[] { "AL36", "AL35" }, InputSheet),
                        BoxWidth = (new string[] { "AM42", "AL42" }, InputSheet),
                        TubesheetTHK = (new string[] { "AM49", "AL49" }, InputSheet),
                        PlugsheetTHK = (new string[] { "AM50", "AL50" }, InputSheet),
                        TopAndBottomPlateTHK = (new string[] { "AM51", "AL51" }, InputSheet),
                        VerticalSpan = (new string[] { "CP183", }, SketchCalcsSheet),
                        BoxLength = (new string[] { "BQ44" }, InputSheet),
                        Y_Location = (new string[] { "BGM40" }, InputsCalcsSheet),
                        Xtop = (new string[] { "AM47", "AL47" }, InputSheet),
                    },
                    ["63"] = new Prego_DTO
                    {
                        HeaderRequired = (new string[] { "AG36", "AG35" }, InputSheet),
                        BoxWidth = (new string[] { "AI42", "AG42" }, InputSheet),
                        TubesheetTHK = (new string[] { "AI49", "AG49" }, InputSheet),
                        PlugsheetTHK = (new string[] { "AI50", "AG50" }, InputSheet),
                        TopAndBottomPlateTHK = (new string[] { "AI51", "AG51" }, InputSheet),
                        VerticalSpan = (new string[] { "CP181", }, SketchCalcsSheet),
                        BoxLength = (new string[] { "BQ44" }, InputSheet),
                        Y_Location = (new string[] { "BGM38" }, InputsCalcsSheet)
                    },
                    ["64"] = new Prego_DTO
                    {
                        HeaderRequired = (new string[] { "AO36", "AO35" }, InputSheet),
                        BoxWidth = (new string[] { "AQ42", "AO42" }, InputSheet),
                        TubesheetTHK = (new string[] { "AQ49", "AO49" }, InputSheet),
                        PlugsheetTHK = (new string[] { "AQ50", "AO50" }, InputSheet),
                        TopAndBottomPlateTHK = (new string[] { "AQ51", "AO51" }, InputSheet),
                        VerticalSpan = (new string[] { "CP184", }, SketchCalcsSheet),
                        BoxLength = (new string[] { "BQ44" }, InputSheet),
                        Y_Location = (new string[] { "BGM41" }, InputsCalcsSheet)
                    },
                    ["65"] = new Prego_DTO
                    {
                        HeaderRequired = (new string[] { "AJ36", "AJ35" }, InputSheet),
                        BoxWidth = (new string[] { "AK42", "AJ42" }, InputSheet),
                        TubesheetTHK = (new string[] { "AK49", "AJ49" }, InputSheet),
                        PlugsheetTHK = (new string[] { "AK50", "AJ50" }, InputSheet),
                        TopAndBottomPlateTHK = (new string[] { "AK51", "AJ51" }, InputSheet),
                        VerticalSpan = (new string[] { "CP182", }, SketchCalcsSheet),
                        BoxLength = (new string[] { "BQ44" }, InputSheet),
                        Y_Location = (new string[] { "BGM39" }, InputsCalcsSheet)
                    },
                    ["66"] = new Prego_DTO
                    {
                        HeaderRequired = (new string[] { "AR36", "AR35" }, InputSheet),
                        BoxWidth = (new string[] { "AS42", "AR42" }, InputSheet),
                        TubesheetTHK = (new string[] { "AS49", "AR49" }, InputSheet),
                        PlugsheetTHK = (new string[] { "AS50", "AR50" }, InputSheet),
                        TopAndBottomPlateTHK = (new string[] { "AS51", "AR51" }, InputSheet),
                        VerticalSpan = (new string[] { "CP185", }, SketchCalcsSheet),
                        BoxLength = (new string[] { "BQ44" }, InputSheet),
                        Y_Location = (new string[] { "BGM42" }, InputsCalcsSheet)
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
        }

        #endregion  
    }
}
