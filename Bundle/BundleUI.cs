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

namespace Bundle
{
    public partial class BundleUI : Form
    {
        #region Buttons, Constructor, Helper Methods

        public BundleUI()
        {
            InitializeComponent();
        }
        private void bBundle_Click(object sender, EventArgs e)
        {
            new Bundle(7, "Bundle");
        }
        double LoadPregoDouble(TextBox textBox, Worksheet worksheet, params string[] cellNames)
        {
            double value = CellDouble(worksheet, cellNames);
            textBox.Text = value.ToString();
            return value;
        }
        string LoadPregoString(TextBox textBox, Worksheet worksheet, params string[] cellNames)
        {
            string value = CellString(worksheet, cellNames);
            textBox.Text = value;
            return value;
        }

        #endregion

        private void BundleUI_Load(object sender, EventArgs e)
        {
            // Prego imports
            tBundleWidth.Text = Bundle_Width.ToString();
            tSideFrameTHK.Text = SideFrame_THK.ToString();
            tDepth.Text = SideFrame_Depth.ToString();

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
        }

        #region Prego Imports

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
        #region Advanced

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

        private void bImportPrego_Click(object sender, EventArgs e)
        {
            if (InputSheet != null)
            {
                Customer = LoadPregoString(customer_Box, InputSheet, 
                    "B" + 2); // Customer:
                PlantLocation = LoadPregoString(location_Box, InputSheet,
                    "B" + 4); // Plant:
                PurchaseOrder = LoadPregoString(purchaseOrder_Box, InputSheet,
                    "B" + 5); // PO No:
                ItemNumber = LoadPregoString(itemNumber_Box, InputSheet,
                    "H" + 3); // Item:

                Bundle_Width = LoadPregoDouble(tBundleWidth, InputSheet,
                    "BQ" + 45); // Bdl Wd/Toed:
                SideFrame_Depth = LoadPregoDouble(tDepth, InputSheet,
                    "CG" + 30, // override
                    "CF" + 30);// Frame Depth (in)
                SideFrame_THK = LoadPregoDouble(tSideFrameTHK, InputSheet,
                    "CG" + 32, // override
                    "CF" + 32);// Frame Thk (in)

                MessageBox.Show($"Data imported from Prego successfully", "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
            else
            {
                MessageBox.Show("Prego file not found", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }
    }
}
