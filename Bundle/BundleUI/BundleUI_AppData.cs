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
        // UI Data structure
        public class Header_AppData
        {
            public IHeaderExtensions Header { get; set; }
            public CheckBox CheckBox { get; set; }
            public TextBox BoxWidthTextBox { get; set; }
            public TextBox TubesheetTHKTextBox { get; set; }
            public TextBox PlugsheetTHKTextBox { get; set; }
        }


        // Map application data to UI controls
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
                    PlugsheetTHKTextBox = tPlugsheetTHK_61
                }
                },
                { "62", new Header_AppData
                {
                    Header = Header62,
                    CheckBox = cEnabled62,
                    BoxWidthTextBox = tBoxWidth62,
                    TubesheetTHKTextBox = tTubesheetTHK_62,
                    PlugsheetTHKTextBox = tPlugsheetTHK_62
                }
                },
                { "63", new Header_AppData
                {
                        Header = Header63,
                        CheckBox = cEnabled63,
                        BoxWidthTextBox = tBoxWidth63,
                        TubesheetTHKTextBox = tTubesheetTHK_63,
                        PlugsheetTHKTextBox = tPlugsheetTHK_63
                    }
                },
                { "64", new Header_AppData
                {
                        Header = Header64,
                        CheckBox = cEnabled64,
                        BoxWidthTextBox = tBoxWidth64,
                        TubesheetTHKTextBox = tTubesheetTHK_64,
                        PlugsheetTHKTextBox = tPlugsheetTHK_64
                    }
                },
                { "65", new Header_AppData
                {
                        Header = Header65,
                        CheckBox = cEnabled65,
                        BoxWidthTextBox = tBoxWidth65,
                        TubesheetTHKTextBox = tTubesheetTHK_65,
                        PlugsheetTHKTextBox = tPlugsheetTHK_65
                    }
                },
                { "66", new Header_AppData
                {
                        Header = Header66,
                        CheckBox = cEnabled66,
                        BoxWidthTextBox = tBoxWidth66,
                        TubesheetTHKTextBox = tTubesheetTHK_66,
                        PlugsheetTHKTextBox = tPlugsheetTHK_66
                    }
                }
            };
            }
        }

        
        // Update UI
        private void LoadHeaderData_FromApp(string headerNumber)
        {
            var headerControls = _headerAppData[headerNumber];

            headerControls.CheckBox.Checked = headerControls.Header.IsRequired;
            headerControls.BoxWidthTextBox.Enabled = headerControls.Header.IsRequired;
            headerControls.TubesheetTHKTextBox.Enabled = headerControls.Header.IsRequired;
            headerControls.PlugsheetTHKTextBox.Enabled = headerControls.Header.IsRequired;

            if (headerControls.Header.IsRequired)
            {
                headerControls.BoxWidthTextBox.Text = headerControls.Header.BoxWidth.ToString();
                headerControls.TubesheetTHKTextBox.Text = headerControls.Header.TubesheetTHK.ToString();
                headerControls.PlugsheetTHKTextBox.Text = headerControls.Header.PlugsheetTHK.ToString();
            }
            else
            {
                headerControls.BoxWidthTextBox.Text = "";
                headerControls.TubesheetTHKTextBox.Text = "";
                headerControls.PlugsheetTHKTextBox.Text = "";
            }
        }
    }
}
