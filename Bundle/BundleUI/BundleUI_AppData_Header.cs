using FileTools.Base;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using static FileTools.CommonData.CommonData;

namespace Bundle
{
    public partial class BundleUI
    {
        // UI data structure
        public class Header_AppData
        {
            public IHeaderExtensions Header { get; set; }
            public CheckBox CheckBox { get; set; }
            public TextBox BoxWidthTextBox { get; set; }
            public TextBox TubesheetTHKTextBox { get; set; }
            public TextBox PlugsheetTHKTextBox { get; set; }
            public TextBox TopBottomTHKTextBox { get; set; }
            public TextBox BoxLengthTextBox { get; set; }
            public TextBox VerticalSpanTextBox { get; set; }
        }


        // Map application data to UI
        Dictionary<string, Header_AppData> _headerAppData
        {
            get
            {
                return new Dictionary<string, Header_AppData>
            {
                { "61", new Header_AppData
                {
                    Header = Header61,
                    CheckBox = cEnabled61,
                    BoxWidthTextBox = tBoxWidth61,
                    TubesheetTHKTextBox = tTubesheetTHK_61,
                    PlugsheetTHKTextBox = tPlugsheetTHK_61,
                    TopBottomTHKTextBox = tTopBottomTHK_61,
                    BoxLengthTextBox = tBoxLength_61,
                    VerticalSpanTextBox = tVerticalSpan_61,
                }
                },
                { "62", new Header_AppData
                {
                    Header = Header62,
                    CheckBox = cEnabled62,
                    BoxWidthTextBox = tBoxWidth62,
                    TubesheetTHKTextBox = tTubesheetTHK_62,
                    PlugsheetTHKTextBox = tPlugsheetTHK_62,
                    TopBottomTHKTextBox = tTopBottomTHK_62,
                    BoxLengthTextBox = tBoxLength_62,
                    VerticalSpanTextBox = tVerticalSpan_62,
                }
                },
                { "63", new Header_AppData
                {
                    Header = Header63,
                    CheckBox = cEnabled63,
                    BoxWidthTextBox = tBoxWidth63,
                    TubesheetTHKTextBox = tTubesheetTHK_63,
                    PlugsheetTHKTextBox = tPlugsheetTHK_63,
                    TopBottomTHKTextBox = tTopBottomTHK_63,
                    BoxLengthTextBox = tBoxLength_63,
                    VerticalSpanTextBox = tVerticalSpan_63,
                }
                },
                { "64", new Header_AppData
                {
                    Header = Header64,
                    CheckBox = cEnabled64,
                    BoxWidthTextBox = tBoxWidth64,
                    TubesheetTHKTextBox = tTubesheetTHK_64,
                    PlugsheetTHKTextBox = tPlugsheetTHK_64,
                    TopBottomTHKTextBox = tTopBottomTHK_64,
                    BoxLengthTextBox = tBoxLength_64,
                    VerticalSpanTextBox = tVerticalSpan_64,
                }
                },
                { "65", new Header_AppData
                {
                    Header = Header65,
                    CheckBox = cEnabled65,
                    BoxWidthTextBox = tBoxWidth65,
                    TubesheetTHKTextBox = tTubesheetTHK_65,
                    PlugsheetTHKTextBox = tPlugsheetTHK_65,
                    TopBottomTHKTextBox = tTopBottomTHK_65,
                    BoxLengthTextBox = tBoxLength_65,
                    VerticalSpanTextBox = tVerticalSpan_65,
                }
                },
                { "66", new Header_AppData
                {
                    Header = Header66,
                    CheckBox = cEnabled66,
                    BoxWidthTextBox = tBoxWidth66,
                    TubesheetTHKTextBox = tTubesheetTHK_66,
                    PlugsheetTHKTextBox = tPlugsheetTHK_66,
                    TopBottomTHKTextBox = tTopBottomTHK_66,
                    BoxLengthTextBox = tBoxLength_66,
                    VerticalSpanTextBox = tVerticalSpan_66,
                }
                }
            };
            }
        }


        // Push application data to UI
        private void LoadHeaderData_FromApp(string headerNumber)
        {
            var headerControls = _headerAppData[headerNumber];
            headerControls.CheckBox.Checked = headerControls.Header.IsRequired;

            ToggleTextbox(headerControls);

            if (headerControls.Header.IsRequired)
                PushDataToTextbox(headerControls);
            else
                HideTextboxData(headerControls);
        }

        void ToggleTextbox(Header_AppData headerControls)
        {
            foreach (var property in headerControls.GetType().GetProperties())
            {
                if (property.PropertyType == typeof(TextBox))
                {
                    TextBox textBox = (TextBox)property.GetValue(headerControls);
                    textBox.Enabled = headerControls.Header.IsRequired;
                }
            }
        }
        void PushDataToTextbox(Header_AppData headerControls)
        {
            foreach (var property in headerControls.Header.GetType().GetProperties())
            {
                if (property.PropertyType == typeof(double))
                {
                    double value = (double)property.GetValue(headerControls.Header);
                    TextBox textBox = (TextBox)headerControls.GetType().GetProperty(property.Name + "TextBox").GetValue(headerControls);
                    textBox.Text = value.ToString();
                }
            }
        }
        void HideTextboxData(Header_AppData headerControls)
        {
            foreach (var property in headerControls.GetType().GetProperties())
            {
                if (property.PropertyType == typeof(TextBox))
                {
                    TextBox textBox = (TextBox)property.GetValue(headerControls);
                    textBox.Text = "";
                }
            }
        }
    }
}
