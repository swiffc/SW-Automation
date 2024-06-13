using static Excel.Prego;
using System;
using System.Windows.Forms;
using static FileTools.CommonData.CommonData;
using static FileTools.Properties.Settings;
using Microsoft.Office.Interop.Excel;
using TextBox = System.Windows.Forms.TextBox;
using ComboBox = System.Windows.Forms.ComboBox;
using static System.Windows.Forms.VisualStyles.VisualStyleElement;
using SplashScreen;
using Excel;
using static FileTools.StaticFileTools;
using System.Runtime.Remoting.Messaging;
using CheckBox = System.Windows.Forms.CheckBox;
using System.Collections.Generic;
using FileTools.Base;
using static Bundle.BundleUI;
using SolidWorks.Interop.sldworks;

namespace Bundle
{
    public partial class BundleUI : Form
    {
        public BundleUI()
        {
            InitializeComponent();
        }
        #region Events

        private void BundleUI_Load(object sender, EventArgs e)
        {
            // Prego imports
            tBundleWidth.Text = Bundle_Width.ToString();
            cSideFrameTHK.Text = SideFrame_THK.ToString();
            tDepth.Text = SideFrame_Depth.ToString();
            cHeadersOutsideFrame.Checked = HeadersOutsideFrames;
            tTubeLength.Text = TubeLength.ToString();
            tTubeProjection.Text = TubeProjection.ToString();

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
        private void BundleUI_FormClosing(object sender, FormClosingEventArgs e)
        {
            PleaseWait.Stop();
        }

        #endregion
        #region Buttons

        private void bImportPrego_Click(object sender, EventArgs e)
        {
            if (InputSheet != null)
            {
                // Job info
                Customer = LoadPregoValue<string>(customer_Box, InputSheet,
                    "B" + 2); // Customer:
                Client = LoadPregoValue<string>(client_Box, InputSheet,
                    "B" + 2); // Customer:
                PlantLocation = LoadPregoValue<string>(location_Box, InputSheet,
                    "B" + 4); // Plant:
                PurchaseOrder = LoadPregoValue<string>(purchaseOrder_Box, InputSheet,
                    "B" + 5); // PO No:
                ItemNumber = LoadPregoValue<string>(itemNumber_Box, InputSheet,
                    "H" + 3); // Item:


                // Bundle
                Bundle_Width = LoadPregoDouble(tBundleWidth, InputSheet,
                    "BQ" + 45); // Bdl Wd/Toed:
                HeadersAreOutsideTheFrame = LoadPregoBool(cHeadersOutsideFrame, InputSheet,
                    "G" + 15, // Override
                    "F" + 15);// Hdrs outside Fr?
                TubeLength = LoadPregoDouble_FeetToInches(tTubeLength, InputSheet,
                    "L" + 15, // Override
                    "N" + 15);// Tube Length (ft)
                TubeProjection = LoadPregoDouble(tTubeProjection, InputSheet,
                    0.25,  // SE
                    0.125);// HPC


                // SideFrame
                SideFrame_Depth = LoadPregoDouble(tDepth, InputSheet,
                    "CG" + 30, // Override
                    "CF" + 30);// Frame Depth (in)
                SideFrame_THK = LoadPregoValue<double>(cSideFrameTHK, InputSheet,
                    "CG" + 32, // Override
                    "CF" + 32);// Frame Thk (in)


                ImportHeaderData_FromPrego();

                SaveSettings();
                MessageBox.Show($"Data imported from Prego successfully", "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
            else
            {
                MessageBox.Show("Prego file not found", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }
        private void bBundle_Click(object sender, EventArgs e)
        {
            if (!Developer)
            {
                SignInitials();
            }
            new Bundle(7, "Bundle");
        }

        #endregion
        #region Static Helpers

        static double LoadPregoDouble(TextBox textBox, Worksheet worksheet, double valueIfSmithco, double ValueIfHPC)
        {
            string[] cellNames = new string[]
            {
                "G" + 27, // Override
                "F" + 27 // Titleblock Manuf
            };

            string titleblockManuf = CellString(worksheet, cellNames);

            if (titleblockManuf == "Smithco")
            {
                textBox.Text = valueIfSmithco.ToString();
                return valueIfSmithco;
            }
            else
            {
                textBox.Text = ValueIfHPC.ToString();
                return ValueIfHPC;
            }
        }
        static double LoadPregoDouble_FeetToInches(TextBox textBox, Worksheet worksheet, params string[] cellNames)
        {
            string tubeLength_ArchitecturalFeet = CellString(worksheet, cellNames[0]);
            double tubeLength_DecimalFeet = CellDouble(worksheet, cellNames[1]);

            if (tubeLength_ArchitecturalFeet != null)
            {
                // #'-#" --> decimal inches
                var parts = tubeLength_ArchitecturalFeet.Split('\'', '\"');
                double feet = double.Parse(parts[0]);
                double inches = double.Parse(parts[1]);
                double decimalInches = feet * 12 + inches;

                textBox.Text = decimalInches.ToString();
                return decimalInches;
            }
            else // Decimal feet --> decimal inches
            {
                double decimalInches = tubeLength_DecimalFeet * 12;
                textBox.Text = decimalInches.ToString();
                return decimalInches;
            }
        }
        static double LoadPregoDouble(TextBox textBox, Worksheet worksheet, params string[] cellNames)
        {
            double value = CellDouble(worksheet, cellNames);
            textBox.Text = value.ToString();
            return value;
        }
        static T LoadPregoValue<T>(Control control, Worksheet worksheet, params string[] cellNames)
            where T : IConvertible
        {
            var value = CellString(worksheet, cellNames);
            if (value == null)
            {
                value = CellDouble(worksheet, cellNames).ToString();
            }
            control.Text = value;
            return (T)Convert.ChangeType(value, typeof(T));
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

        #endregion
        #region UpdateUI

        #region UpdateUI_Headers
        private void tTopBottomTHK_61_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTopBottomTHK_61.Text, x => Header61.TopAndBottomPlateTHK = x);
        }

        private void tTopBottomTHK_62_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTopBottomTHK_62.Text, x => Header62.TopAndBottomPlateTHK = x);
        }

        private void tTopBottomTHK_63_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTopBottomTHK_63.Text, x => Header63.TopAndBottomPlateTHK = x);
        }

        private void tTopBottomTHK_64_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTopBottomTHK_64.Text, x => Header64.TopAndBottomPlateTHK = x);
        }

        private void tTopBottomTHK_65_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTopBottomTHK_65.Text, x => Header65.TopAndBottomPlateTHK = x);
        }

        private void tTopBottomTHK_66_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTopBottomTHK_66.Text, x => Header66.TopAndBottomPlateTHK = x);
        }
        private void tBoxLength_61_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxLength_61.Text, x => Header61.BoxLength = x);
        }

        private void tBoxLength_62_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxLength_62.Text, x => Header62.BoxLength = x);
        }

        private void tBoxLength_63_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxLength_63.Text, x => Header63.BoxLength = x);
        }

        private void tBoxLength_64_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxLength_64.Text, x => Header64.BoxLength = x);
        }

        private void tBoxLength_65_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxLength_65.Text, x => Header65.BoxLength = x);
        }

        private void tBoxLength_66_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxLength_66.Text, x => Header66.BoxLength = x);
        }
        private void tVerticalSpan_61_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tVerticalSpan_61.Text, x => Header61.VerticalSpan = x);
        }

        private void tVerticalSpan_62_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tVerticalSpan_62.Text, x => Header62.VerticalSpan = x);
        }

        private void tVerticalSpan_63_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tVerticalSpan_63.Text, x => Header63.VerticalSpan = x);
        }

        private void tVerticalSpan_64_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tVerticalSpan_64.Text, x => Header64.VerticalSpan = x);
        }

        private void tVerticalSpan_65_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tVerticalSpan_65.Text, x => Header65.VerticalSpan = x);
        }

        private void tVerticalSpan_66_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tVerticalSpan_66.Text, x => Header66.VerticalSpan = x);
        }
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
        #region UpdateUI_Bundle

        private void tTubeLength_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubeLength.Text, x => TubeLength = x);
        }

        private void tTubeProjection_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubeProjection.Text, x => TubeProjection = x);
        }
        private void cHeadersOutsideFrame_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cHeadersOutsideFrame.Checked, x => HeadersOutsideFrames = x);
        }
        private void tWidth_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBundleWidth.Text, x => Bundle_Width = x);
        }

        private void cSideFrameTHK_Leave(object sender, EventArgs e)
        {
            cSideFrameTHK.Text = SideFrame_THK.ToString();
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
        #region UpdateUI_AdvancedTab

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

        #endregion


    }
}
