using FileTools.Base;
using Microsoft.Office.Interop.Excel;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Excel.Prego;
using CheckBox = System.Windows.Forms.CheckBox;
using TextBox = System.Windows.Forms.TextBox;

namespace Bundle
{
    public partial class BundleUI
    {
        // Prego cell data structure
        public class Header_PregoData
        {
            public (string[] Cells, Worksheet Sheet) HeaderRequired { get; set; }
            public (string[] Cells, Worksheet Sheet) BoxWidth { get; set; }
            public (string[] Cells, Worksheet Sheet) TubesheetTHK { get; set; }
            public (string[] Cells, Worksheet Sheet) PlugsheetTHK { get; set; }
            public (string[] Cells, Worksheet Sheet) TopAndBottomTHK { get; set; }
            public (string[] Cells, Worksheet Sheet) VerticalSpan { get; set; }
            public (string[] Cells, Worksheet Sheet) BoxLength { get; set; }
        }


        // Map Prego data
        Dictionary<string, Header_PregoData> _headerPregoData
        {
            get
            {
                return new Dictionary<string, Header_PregoData>
                {
                    ["61"] = new Header_PregoData
                    {
                        HeaderRequired = (new string[] { "AD36", "AD35" }, InputSheet),
                        BoxWidth = (new string[] { "AE42", "AD42" }, InputSheet),
                        TubesheetTHK = (new string[] { "AE49", "AD49" }, InputSheet),
                        PlugsheetTHK = (new string[] { "AE50", "AD50" }, InputSheet),
                        TopAndBottomTHK = (new string[] { "", "" }, InputSheet),/////////////////////////////////////////////
                        VerticalSpan = (new string[] { "", "" }, InputSheet),////////////////////////////////////////////////
                        BoxLength = (new string[] { "", "" }, InputSheet),///////////////////////////////////////////////////
                    },
                    ["62"] = new Header_PregoData
                    {
                        HeaderRequired = (new string[] { "AL36", "AL35" }, InputSheet),
                        BoxWidth = (new string[] { "AM42", "AL42" }, InputSheet),
                        TubesheetTHK = (new string[] { "AM49", "AL49" }, InputSheet),
                        PlugsheetTHK = (new string[] { "AM50", "AL50" }, InputSheet),
                        TopAndBottomTHK = (new string[] { "", "" }, InputSheet),/////////////////////////////////////////////
                        VerticalSpan = (new string[] { "", "" }, InputSheet),////////////////////////////////////////////////
                        BoxLength = (new string[] { "", "" }, InputSheet),///////////////////////////////////////////////////
                    },
                    ["63"] = new Header_PregoData
                    {
                        HeaderRequired = (new string[] { "AG36", "AG35" }, InputSheet),
                        BoxWidth = (new string[] { "AI42", "AG42" }, InputSheet),
                        TubesheetTHK = (new string[] { "AI49", "AG49" }, InputSheet),
                        PlugsheetTHK = (new string[] { "AI50", "AG50" }, InputSheet),
                        TopAndBottomTHK = (new string[] { "", "" }, InputSheet),/////////////////////////////////////////////
                        VerticalSpan = (new string[] { "", "" }, InputSheet),////////////////////////////////////////////////
                        BoxLength = (new string[] { "", "" }, InputSheet),///////////////////////////////////////////////////
                    },
                    ["64"] = new Header_PregoData
                    {
                        HeaderRequired = (new string[] { "AO36", "AO35" }, InputSheet),
                        BoxWidth = (new string[] { "AQ42", "AO42" }, InputSheet),
                        TubesheetTHK = (new string[] { "AQ49", "AO49" }, InputSheet),
                        PlugsheetTHK = (new string[] { "AQ50", "AO50" }, InputSheet),
                        TopAndBottomTHK = (new string[] { "", "" }, InputSheet),/////////////////////////////////////////////
                        VerticalSpan = (new string[] { "", "" }, InputSheet),////////////////////////////////////////////////
                        BoxLength = (new string[] { "", "" }, InputSheet),///////////////////////////////////////////////////
                    },
                    ["65"] = new Header_PregoData
                    {
                        HeaderRequired = (new string[] { "AJ36", "AJ35" }, InputSheet),
                        BoxWidth = (new string[] { "AK42", "AJ42" }, InputSheet),
                        TubesheetTHK = (new string[] { "AK49", "AJ49" }, InputSheet),
                        PlugsheetTHK = (new string[] { "AK50", "AJ50" }, InputSheet),
                        TopAndBottomTHK = (new string[] { "", "" }, InputSheet),/////////////////////////////////////////////
                        VerticalSpan = (new string[] { "", "" }, InputSheet),////////////////////////////////////////////////
                        BoxLength = (new string[] { "", "" }, InputSheet),///////////////////////////////////////////////////
                    },
                    ["66"] = new Header_PregoData
                    {
                        HeaderRequired = (new string[] { "AR36", "AR35" }, InputSheet),
                        BoxWidth = (new string[] { "AS42", "AR42" }, InputSheet),
                        TubesheetTHK = (new string[] { "AS49", "AR49" }, InputSheet),
                        PlugsheetTHK = (new string[] { "AS50", "AR50" }, InputSheet),
                        TopAndBottomTHK = (new string[] { "", "" }, InputSheet),/////////////////////////////////////////////
                        VerticalSpan = (new string[] { "", "" }, InputSheet),////////////////////////////////////////////////
                        BoxLength = (new string[] { "", "" }, InputSheet),///////////////////////////////////////////////////
                    }
                };
            }
        }




        /////////////////////////////////////////////////
        ///////////////Automate Below////////////////////
        /////////////////////////////////////////////////
       



        // Push Prego data to application
        private void LoadHeaderData_FromPrego(IHeaderExtensions header, CheckBox checkBox, TextBox boxWidthTextBox, TextBox tubesheetTHKTextBox, TextBox plugsheetTHKTextBox, Header_PregoData prego)
        {
            header.IsRequired = LoadPregoBool_NullOrEmpty(checkBox, prego.HeaderRequired.Sheet, prego.HeaderRequired.Cells);
            if (header.IsRequired)
            {
                header.BoxWidth = LoadPregoDouble(boxWidthTextBox, prego.BoxWidth.Sheet, prego.BoxWidth.Cells);
                header.TubesheetTHK = LoadPregoDouble(tubesheetTHKTextBox, prego.TubesheetTHK.Sheet, prego.TubesheetTHK.Cells);
                header.PlugsheetTHK = LoadPregoDouble(plugsheetTHKTextBox, prego.PlugsheetTHK.Sheet, prego.PlugsheetTHK.Cells);
            }
        }


        // Push application data to UI
        private void ImportHeaderData_FromPrego()
        {
            var headerNumbers = new[] { "61", "62", "63", "64", "65", "66" };
            foreach (var headerNumber in headerNumbers)
            {
                var headerAppData = _headerAppData[headerNumber];
                var headerPregoData = _headerPregoData[headerNumber];
                LoadHeaderData_FromPrego
                (
                    headerAppData.Header,
                    headerAppData.CheckBox,
                    headerAppData.BoxWidthTextBox,
                    headerAppData.TubesheetTHKTextBox,
                    headerAppData.PlugsheetTHKTextBox,
                    headerPregoData
                );
            }
        }

        

    }
}
