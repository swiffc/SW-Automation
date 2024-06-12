using static Excel.Prego;
using System;
using System.Windows.Forms;
using static FileTools.CommonData.CommonData;
using static FileTools.Properties.Settings;
using Microsoft.Office.Interop.Excel;
using TextBox = System.Windows.Forms.TextBox;
using static System.Windows.Forms.VisualStyles.VisualStyleElement;
using SplashScreen;
using Excel;
using static FileTools.StaticFileTools;
using System.Runtime.Remoting.Messaging;
using CheckBox = System.Windows.Forms.CheckBox;
using System.Collections.Generic;
using FileTools.Base;
using static Bundle.BundleUI;

namespace Bundle
{
    public partial class BundleUI : Form
    {
        #region Header Data Structure

        
        Dictionary<string, Header_PregoData> _headerPregoData;
        public BundleUI()
        {
            InitializeComponent();

            _headerPregoData = new Dictionary<string, Header_PregoData>
            {
                ["61"] = new Header_PregoData
                {
                    IsRequiredCells = new string[] 
                    { 
                        "AD36", 
                        "AD35" 
                    },
                    BoxWidthCells = new string[] 
                    { 
                        "AE42", 
                        "AD42" 
                    },
                    TubesheetTHKCells = new string[] 
                    { 
                        "AE49",
                        "AD49" 
                    },
                    PlugsheetTHKCells = new string[] 
                    { 
                        "AE50", 
                        "AD50" 
                    }
                },
                ["62"] = new Header_PregoData
                {
                    IsRequiredCells = new string[] 
                    {
                        "AL36",
                        "AL35" 
                    },
                    BoxWidthCells = new string[] 
                    {
                        "AM42", 
                        "AL42" 
                    },
                    TubesheetTHKCells = new string[] 
                    {
                        "AM49", 
                        "AL49" 
                    },
                    PlugsheetTHKCells = new string[] 
                    {
                        "AM50", 
                        "AL50" 
                    }
                },
                ["63"] = new Header_PregoData
                {
                    IsRequiredCells = new string[] { "AG36", "AG35" },
                    BoxWidthCells = new string[] { "AI42", "AG42" },
                    TubesheetTHKCells = new string[] { "AI49", "AG49" },
                    PlugsheetTHKCells = new string[] { "AI50", "AG50" }
                },
                ["64"] = new Header_PregoData
                {
                    IsRequiredCells = new string[] { "AO36", "AO35" },
                    BoxWidthCells = new string[] { "AQ42", "AO42" },
                    TubesheetTHKCells = new string[] { "AQ49", "AO49" },
                    PlugsheetTHKCells = new string[] { "AQ50", "AO50" }
                },
                ["65"] = new Header_PregoData
                {
                    IsRequiredCells = new string[] { "AJ36", "AJ35" },
                    BoxWidthCells = new string[] { "AK42", "AJ42" },
                    TubesheetTHKCells = new string[] { "AK49", "AJ49" },
                    PlugsheetTHKCells = new string[] { "AK50", "AJ50" }
                },
                ["66"] = new Header_PregoData
                {
                    IsRequiredCells = new string[] { "AR36", "AR35" },
                    BoxWidthCells = new string[] { "AS42", "AR42" },
                    TubesheetTHKCells = new string[] { "AS49", "AR49" },
                    PlugsheetTHKCells = new string[] { "AS50", "AR50" }
                }
            };
        }
        public class Header_PregoData
        {
            public string[] IsRequiredCells { get; set; }
            public string[] BoxWidthCells { get; set; }
            public string[] TubesheetTHKCells { get; set; }
            public string[] PlugsheetTHKCells { get; set; }
        }
        private void LoadHeaderData_FromPrego(IHeaderExtensions header, CheckBox checkBox, TextBox boxWidthTextBox, TextBox tubesheetTHKTextBox, TextBox plugsheetTHKTextBox, Worksheet inputSheet, Header_PregoData cellReferences)
        {
            header.IsRequired = LoadPregoBool_NullOrEmpty(checkBox, inputSheet, cellReferences.IsRequiredCells);
            if (header.IsRequired)
            {
                header.BoxWidth = LoadPregoDouble(boxWidthTextBox, inputSheet, cellReferences.BoxWidthCells);
                header.TubesheetTHK = LoadPregoDouble(tubesheetTHKTextBox, inputSheet, cellReferences.TubesheetTHKCells);
                header.PlugsheetTHK = LoadPregoDouble(plugsheetTHKTextBox, inputSheet, cellReferences.PlugsheetTHKCells);
            }
        }
        private void ImportHeaderData_FromPrego()
        {
            var headerNumbers = new[] { "61", "62", "63", "64", "65", "66" };
            foreach (var headerNumber in headerNumbers)
            {
                var headerAppData = _headerAppData[headerNumber];
                var headerCellReferences = _headerPregoData[headerNumber];
                LoadHeaderData_FromPrego
                (
                    headerAppData.Header, 
                    headerAppData.CheckBox, 
                    headerAppData.BoxWidthTextBox, 
                    headerAppData.TubesheetTHKTextBox, 
                    headerAppData.PlugsheetTHKTextBox, 
                    InputSheet, 
                    headerCellReferences
                );
            }
        }

        #endregion

        #region Buttons, Events, Private Methods

        private void BundleUI_Load(object sender, EventArgs e)
        {
            // Prego imports
            tBundleWidth.Text = Bundle_Width.ToString();
            tSideFrameTHK.Text = SideFrame_THK.ToString();
            tDepth.Text = SideFrame_Depth.ToString();
            cHeadersOutsideFrame.Checked = HeadersOutsideFrames;

            // Advanced
            createDrawing_Toggle.Checked = Default.Toggle_CreateDrawing;
            save_Toggle.Checked = Default.Toggle_Save;
            delete_Toggle.Checked = Default.Toggle_DeleteFiles;

            // Job
            textBox_Bank.Text = Bank.ToString();
            job_Box.Text = Project;
            customer_Box.Text = Customer;
            client_Box.Text = Client;
            location_Box.Text = PlantLocation;
            purchaseOrder_Box.Text = PurchaseOrder;
            itemNumber_Box.Text = ItemNumber;
            initials_Box.Text = Initials;

            // Headers
            LoadHeaderData_FromApp("61");
            LoadHeaderData_FromApp("62");
            LoadHeaderData_FromApp("63");
            LoadHeaderData_FromApp("64");
            LoadHeaderData_FromApp("65");
            LoadHeaderData_FromApp("66");
        }

        private void bBundle_Click(object sender, EventArgs e)
        {
            if (!Developer)
            {
                SignInitials();
            }
            new Bundle(7, "Bundle");
        }
        static double LoadPregoDouble(TextBox textBox, Worksheet worksheet, params string[] cellNames)
        {
            double value = CellDouble(worksheet, cellNames);
            textBox.Text = value.ToString();
            return value;
        }
        static string LoadPregoString(TextBox textBox, Worksheet worksheet, params string[] cellNames)
        {
            string value = CellString(worksheet, cellNames);
            textBox.Text = value;
            return value;
        }
        static bool LoadPregoBool_NullOrEmpty(CheckBox checkBox, Worksheet worksheet, params string[] cellNames)
        {
            string value = CellString(worksheet, cellNames);
            bool enabled = value == null ? false : true;
            checkBox.Checked = enabled;
            return enabled;
        }
        static bool LoadPregoBool(CheckBox checkBox, Worksheet worksheet, params string[] cellNames)
        {
            string value = CellString(worksheet, cellNames).ToLower();
            bool enabled;
            if (value == "yes" || value == "true")
            {
                enabled = true;
            }
            else if (value == "no" || value == "false")
            {
                enabled = false;
            }
            else
            {
                throw new FormatException("The cell does not contain a recognized boolean value.");
            }
            checkBox.Checked = enabled;
            return enabled;
        }
        private void BundleUI_FormClosing(object sender, FormClosingEventArgs e)
        {
            PleaseWait.Stop();
        }

        #endregion

        #region Prego_Imports

        private void bImportPrego_Click(object sender, EventArgs e)
        {
            if (InputSheet != null)
            {
                // Job info
                Customer = LoadPregoString(customer_Box, InputSheet,
                    "B" + 2); // Customer:
                Client = LoadPregoString(client_Box, InputSheet,
                    "B" + 2); // Customer:
                PlantLocation = LoadPregoString(location_Box, InputSheet,
                    "B" + 4); // Plant:
                PurchaseOrder = LoadPregoString(purchaseOrder_Box, InputSheet,
                    "B" + 5); // PO No:
                ItemNumber = LoadPregoString(itemNumber_Box, InputSheet,
                    "H" + 3); // Item:


                // Bundle
                Bundle_Width = LoadPregoDouble(tBundleWidth, InputSheet,
                    "BQ" + 45); // Bdl Wd/Toed:
                HeadersAreOutsideTheFrame = LoadPregoBool(cHeadersOutsideFrame, InputSheet,
                    "G" + 15, // Override
                    "F" + 15);// Hdrs outside Fr?


                // SideFrame
                SideFrame_Depth = LoadPregoDouble(tDepth, InputSheet,
                    "CG" + 30, // Override
                    "CF" + 30);// Frame Depth (in)
                SideFrame_THK = LoadPregoDouble(tSideFrameTHK, InputSheet,
                    "CG" + 32, // Override
                    "CF" + 32);// Frame Thk (in)


                ImportHeaderData_FromPrego();

                MessageBox.Show($"Data imported from Prego successfully", "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
            else
            {
                MessageBox.Show("Prego file not found", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        #endregion

        #region BDL_UserInputs

        private void cHeadersOutsideFrame_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cHeadersOutsideFrame.Checked, x => HeadersOutsideFrames = x);
        }
        private void tWidth_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBundleWidth.Text, x => Bundle_Width = x);
        }

        private void tSideFrameTHK_Leave(object sender, EventArgs e)
        {
            UI_DoubleChanged(tSideFrameTHK.Text, x => SideFrame_THK = x);
            tSideFrameTHK.Text = SideFrame_THK.ToString();
        }
        private void tDepth_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tDepth.Text, x => SideFrame_Depth = x);
        }

        private void textBox_Bank_TextChanged(object sender, EventArgs e)
        {
            string text = textBox_Bank.Text.ToUpper();
            textBox_Bank.Text = text;
            UI_CharChanged(text, x => Bank = x);
        }
        private void job_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(job_Box.Text, x => Project = x);
        }

        private void customer_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(customer_Box.Text, x => Customer = x);
        }

        private void client_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(client_Box.Text, x => Client = x);
        }

        private void location_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(location_Box.Text, x => PlantLocation = x);
        }

        private void purchaseOrder_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(purchaseOrder_Box.Text, x => PurchaseOrder = x);
        }

        private void itemNumber_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(itemNumber_Box.Text, x => ItemNumber = x);
        }

        private void initials_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(initials_Box.Text, x => Initials = x);
        }

        #endregion
        #region Adv_UserInput

        private void createDrawing_Toggle_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(createDrawing_Toggle.Checked, x => Default.Toggle_CreateDrawing = x);
        }

        private void save_Toggle_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(save_Toggle.Checked, x => Default.Toggle_Save = x);
        }

        private void delete_Toggle_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(delete_Toggle.Checked, x => Default.Toggle_DeleteFiles = x);
        }

        #endregion
        #region HDRs_UserInput
        private void cEnabled61_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled61.Checked, x => Header61.IsRequired = x);
            LoadHeaderData_FromApp("61");
        }

        private void cEnabled62_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled62.Checked, x => Header62.IsRequired = x);
            LoadHeaderData_FromApp("62");
        }

        private void cEnabled63_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled63.Checked, x => Header63.IsRequired = x);
            LoadHeaderData_FromApp("63");
        }

        private void cEnabled64_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled64.Checked, x => Header64.IsRequired = x);
            LoadHeaderData_FromApp("64"); ;
        }

        private void cEnabled65_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled65.Checked, x => Header65.IsRequired = x);
            LoadHeaderData_FromApp("65");
        }

        private void cEnabled66_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled66.Checked, x => Header66.IsRequired = x);
            LoadHeaderData_FromApp("66");
        }
        private void tBoxWidth61_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxWidth61.Text, x => Header61.BoxWidth = x);
        }

        private void tBoxWidth62_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxWidth62.Text, x => Header62.BoxWidth = x);
        }

        private void tBoxWidth63_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxWidth63.Text, x => Header63.BoxWidth = x);
        }

        private void tBoxWidth64_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxWidth64.Text, x => Header64.BoxWidth = x);
        }

        private void tBoxWidth65_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxWidth65.Text, x => Header65.BoxWidth = x);
        }

        private void tBoxWidth66_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxWidth66.Text, x => Header66.BoxWidth = x);
        }
        private void tTubesheetTHK_61_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubesheetTHK_61.Text, x => Header61.TubesheetTHK = x);
        }

        private void tTubesheetTHK_62_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubesheetTHK_62.Text, x => Header62.TubesheetTHK = x);
        }

        private void tTubesheetTHK_63_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubesheetTHK_63.Text, x => Header63.TubesheetTHK = x);
        }

        private void tTubesheetTHK_64_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubesheetTHK_64.Text, x => Header64.TubesheetTHK = x);
        }

        private void tTubesheetTHK_65_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubesheetTHK_65.Text, x => Header65.TubesheetTHK = x);
        }

        private void tTubesheetTHK_66_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubesheetTHK_66.Text, x => Header66.TubesheetTHK = x);
        }
        private void tPlugsheetTHK_61_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tPlugsheetTHK_61.Text, x => Header61.PlugsheetTHK = x);
        }

        private void tPlugsheetTHK_62_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tPlugsheetTHK_62.Text, x => Header62.PlugsheetTHK = x);
        }

        private void tPlugsheetTHK_63_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tPlugsheetTHK_63.Text, x => Header63.PlugsheetTHK = x);
        }

        private void tPlugsheetTHK_64_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tPlugsheetTHK_64.Text, x => Header64.PlugsheetTHK = x);
        }

        private void tPlugsheetTHK_65_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tPlugsheetTHK_65.Text, x => Header65.PlugsheetTHK = x);
        }

        private void tPlugsheetTHK_66_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tPlugsheetTHK_66.Text, x => Header66.PlugsheetTHK = x);
        }

        #endregion


    }
}
