using FileTools.Base;
using SplashScreen;
using System;
using System.Collections.Generic;
using System.Reflection;
using System.Windows.Forms;
using static Excel.Header_DataManager;
using static Excel.Prego;
using static Excel.StaticHelpers;
using static FileTools.CommonData.CommonData;
using static FileTools.Properties.Settings;
using static FileTools.StaticFileTools;

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
            tFrontFinStripBack.Text = FinStripBack_Front.ToString();
            tRearFinStripBack.Text = FinStripBack_Rear.ToString();
            tTubeOD.Text = TubeOD.ToString();
            tTubeWallTHK.Text = TubeWallTHK.ToString();
            tFinOD.Text = FinOD.ToString();
            tTubes_Row_1L.Text = Tube_Row_1L.ToString();
            tTubes_Row_2L.Text = Tube_Row_2L.ToString();
            tTubeHorizPitch.Text = TubeHorizPitch.ToString();
            tTubeQuantity.Text = TubeQuantity.ToString();
            tSlopePerFoot_Row1.Text = Default.SlopePerFoot_Row1.ToString();
            tSlopePerFoot_Row2.Text = Default.SlopePerFoot_Row2.ToString();
            tSlopePerFoot_Row3.Text = Default.SlopePerFoot_Row3.ToString();
            tSlopePerFoot_Row4.Text = Default.SlopePerFoot_Row4.ToString();
            tSlopePerFoot_Row5.Text = Default.SlopePerFoot_Row5.ToString();
            tSlopePerFoot_Row6.Text = Default.SlopePerFoot_Row6.ToString();
            tSlopePerFoot_Row7.Text = Default.SlopePerFoot_Row7.ToString();
            tSlopeAngle_Row8.Text = Default.SlopePerFoot_Row8.ToString();
            tSlopePerFoot_Row9.Text = Default.SlopePerFoot_Row9.ToString();
            tSlopePerFoot_Row10.Text = Default.SlopePerFoot_Row10.ToString();
            tVerticalPitch_1_2.Text = VerticalPitch._1_2.ToString();
            tVerticalPitch_2_3.Text = VerticalPitch._2_3.ToString();
            tVerticalPitch_3_4.Text = VerticalPitch._3_4.ToString();
            tVerticalPitch_4_5.Text = VerticalPitch._4_5.ToString();
            tVerticalPitch_5_6.Text = VerticalPitch._5_6.ToString();
            tVerticalPitch_6_7.Text = VerticalPitch._6_7.ToString();
            tVerticalPitch_7_8.Text = VerticalPitch._7_8.ToString();
            tVerticalPitch_8_9.Text = VerticalPitch._8_9.ToString();
            tVerticalPitch_9_10.Text = VerticalPitch._9_10.ToString();

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
            MapLocal_UI_To_DTO();
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
        private void MapLocal_UI_To_DTO()
        {
            HeaderAppData = new Dictionary<string, UI_DTO>();

            for (int i = 61; i <= 66; i++)
            {
                var uiDto = new UI_DTO
                {
                    Header = GetHeader(i),
                    Enabled = GetControl<CheckBox>("cEnabled", i),
                    BoxWidthTextBox = GetControl<TextBox>("tBoxWidth", i),
                    TubesheetTHKTextBox = GetControl<TextBox>("tTubesheetTHK_", i),
                    PlugsheetTHKTextBox = GetControl<TextBox>("tPlugsheetTHK_", i),
                    TopAndBottomPlateTHKTextBox = GetControl<TextBox>("tTopBottomTHK_", i),
                    BoxLengthTextBox = GetControl<TextBox>("tBoxLength_", i),
                    VerticalSpanTextBox = GetControl<TextBox>("tVerticalSpan_", i),
                    Y_LocationTextBox = GetControl<TextBox>("tY_Location_", i),
                    XtopTextBox = GetControl<TextBox>("Xtop_", i)
                };

                HeaderAppData.Add(i.ToString(), uiDto);
            }
        }
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
                HeadersOutsideFrames = LoadPregoBool(cHeadersOutsideFrame, InputSheet,
                    "G" + 15, // Override
                    "F" + 15);// Hdrs outside Fr?
                TubeLength = LoadPregoDouble_FeetToInches(tTubeLength, InputSheet,
                    "L" + 15, // Override
                    "N" + 15);// Tube Length (ft)
                TubeProjection = LoadPregoDouble(tTubeProjection, InputSheet,
                    0.25,  // SE
                    0.125);// HPC
                FinStripBack_Front = LoadPregoDouble(tFrontFinStripBack, InputSheet,
                    "CP26",
                    "CO26");
                FinStripBack_Rear = LoadPregoDouble(tRearFinStripBack, InputSheet,
                    "CP27",
                    "CO27");
                TubeOD = LoadPregoDouble(tTubeOD, InputSheet,
                    "L10");
                TubeWallTHK = LoadPregoDouble(tTubeWallTHK, InputSheet,
                    "N14",
                    "L14");
                FinOD = LoadPregoDouble(tFinOD, InputSheet,
                    "N19",
                    "L19");
                Tube_Row_1L = LoadPregoInt(tTubes_Row_1L, InputSheet,
                    "AW39",
                    "AU39");
                Tube_Row_2L = LoadPregoInt(tTubes_Row_2L, InputSheet,
                    "AW42",
                    "AU42");
                TubeHorizPitch = LoadPregoDouble(tTubeHorizPitch, InputSheet,
                    "BO47");
                TubeQuantity = LoadPregoInt(tTubeQuantity, InputSheet,
                    "N20",
                    "L20");
                SlopePerFoot.Row1 = LoadPregoDouble(tSlopePerFoot_Row1, SketchCalcsSheet,
                    "CS82");
                SlopePerFoot.Row2 = LoadPregoDouble(tSlopePerFoot_Row2, SketchCalcsSheet,
                    "CS83");
                SlopePerFoot.Row3 = LoadPregoDouble(tSlopePerFoot_Row3, SketchCalcsSheet,
                    "CS84");
                SlopePerFoot.Row4 = LoadPregoDouble(tSlopePerFoot_Row4, SketchCalcsSheet,
                    "CS85");
                SlopePerFoot.Row5 = LoadPregoDouble(tSlopePerFoot_Row5, SketchCalcsSheet,
                    "CS86");
                SlopePerFoot.Row6 = LoadPregoDouble(tSlopePerFoot_Row6, SketchCalcsSheet,
                    "CS87");
                SlopePerFoot.Row7 = LoadPregoDouble(tSlopePerFoot_Row7, SketchCalcsSheet,
                    "CS88");
                SlopePerFoot.Row8 = LoadPregoDouble(tSlopeAngle_Row8, SketchCalcsSheet,
                    "CS89");
                SlopePerFoot.Row9 = LoadPregoDouble(tSlopePerFoot_Row9, SketchCalcsSheet,
                    "CS90");
                SlopePerFoot.Row10 = LoadPregoDouble(tSlopePerFoot_Row10, SketchCalcsSheet,
                    "CS91");
                VerticalPitch._1_2 = LoadPregoDouble(tVerticalPitch_1_2, SketchCalcsSheet,
                    "DF58");
                VerticalPitch._2_3 = LoadPregoDouble(tVerticalPitch_2_3, SketchCalcsSheet,
                    "DF59");
                VerticalPitch._3_4 = LoadPregoDouble(tVerticalPitch_3_4, SketchCalcsSheet,
                    "DF60");
                VerticalPitch._4_5 = LoadPregoDouble(tVerticalPitch_4_5, SketchCalcsSheet,
                    "DF61");
                VerticalPitch._5_6 = LoadPregoDouble(tVerticalPitch_5_6, SketchCalcsSheet,
                    "DF62");
                VerticalPitch._6_7 = LoadPregoDouble(tVerticalPitch_6_7, SketchCalcsSheet,
                    "DF63");
                VerticalPitch._7_8 = LoadPregoDouble(tVerticalPitch_7_8, SketchCalcsSheet,
                    "DF64");
                VerticalPitch._8_9 = LoadPregoDouble(tVerticalPitch_8_9, SketchCalcsSheet,
                    "DF65");
                VerticalPitch._9_10 = LoadPregoDouble(tVerticalPitch_9_10, SketchCalcsSheet,
                    "DF66");



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
        #region Helpers

        private IHeaderExtensions GetHeader(int index)
        {
            switch (index)
            {
                case 61:
                    return Header61;
                case 62:
                    return Header62;
                case 63:
                    return Header63;
                case 64:
                    return Header64;
                case 65:
                    return Header65;
                case 66:
                    return Header66;
                default:
                    throw new ArgumentException($"Invalid index: {index}");
            }
        }
        private T GetControl<T>(string baseName, int index) where T : class
        {
            var controlName = $"{baseName}{index}";
            return this.GetType().GetField(controlName, BindingFlags.Instance | BindingFlags.NonPublic)?.GetValue(this) as T;
        }

        #endregion

        #region UpdateUI

        #region UpdateUI_Headers
        private void Xtop_61_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(Xtop_61.Text, x => Header61.Xtop = x);
        }

        private void Xtop_62_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(Xtop_62.Text, x => Header62.Xtop = x);
        }
        private void tY_Location_61_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tY_Location_61.Text, x => Header61.Y_Location = x);
        }

        private void tY_Location_62_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tY_Location_62.Text, x => Header62.Y_Location = x);
        }

        private void tY_Location_63_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tY_Location_63.Text, x => Header63.Y_Location = x);
        }

        private void tY_Location_64_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tY_Location_64.Text, x => Header64.Y_Location = x);
        }

        private void tY_Location_65_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tY_Location_65.Text, x => Header65.Y_Location = x);
        }

        private void tY_Location_66_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tY_Location_66.Text, x => Header66.Y_Location = x);
        }
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
        private void tVerticalPitch_1_2_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tVerticalPitch_1_2.Text, x => VerticalPitch._1_2 = x);
        }

        private void tVerticalPitch_2_3_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tVerticalPitch_2_3.Text, x => VerticalPitch._2_3 = x);
        }

        private void tVerticalPitch_3_4_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tVerticalPitch_3_4.Text, x => VerticalPitch._3_4 = x);
        }

        private void tVerticalPitch_4_5_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tVerticalPitch_4_5.Text, x => VerticalPitch._4_5 = x);
        }

        private void tVerticalPitch_5_6_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tVerticalPitch_5_6.Text, x => VerticalPitch._5_6 = x);
        }

        private void tVerticalPitch_6_7_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tVerticalPitch_6_7.Text, x => VerticalPitch._6_7 = x);
        }

        private void tVerticalPitch_7_8_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tVerticalPitch_7_8.Text, x => VerticalPitch._7_8 = x);
        }

        private void tVerticalPitch_8_9_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tVerticalPitch_8_9.Text, x => VerticalPitch._8_9 = x);
        }

        private void tVerticalPitch_9_10_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tVerticalPitch_9_10.Text, x => VerticalPitch._9_10 = x);
        }
        private void tSlopeAngle_Row1_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tSlopePerFoot_Row1.Text, x => SlopePerFoot.Row1 = x);
        }

        private void tSlopeAngle_Row2_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tSlopePerFoot_Row2.Text, x => SlopePerFoot.Row2 = x);
        }

        private void tSlopeAngle_Row3_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tSlopePerFoot_Row3.Text, x => SlopePerFoot.Row3 = x);
        }

        private void tSlopeAngle_Row4_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tSlopePerFoot_Row4.Text, x => SlopePerFoot.Row4 = x);
        }

        private void tSlopeAngle_Row5_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tSlopePerFoot_Row5.Text, x => SlopePerFoot.Row5 = x);
        }

        private void tSlopeAngle_Row6_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tSlopePerFoot_Row6.Text, x => SlopePerFoot.Row6 = x);
        }

        private void tSlopeAngle_Row7_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tSlopePerFoot_Row7.Text, x => SlopePerFoot.Row7 = x);
        }

        private void tSlopeAngle_Row9_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tSlopePerFoot_Row9.Text, x => SlopePerFoot.Row9 = x);
        }

        private void tSlopeAngle_Row10_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tSlopePerFoot_Row10.Text, x => SlopePerFoot.Row10 = x);
        }
        private void tTubeQuantity_TextChanged(object sender, EventArgs e)
        {
            UI_IntChanged(tTubeQuantity.Text, x => TubeQuantity = x);
        }
        private void tHorizPitch_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubeHorizPitch.Text, x => TubeHorizPitch = x);
        }
        private void tTubes_Row_1L_TextChanged(object sender, EventArgs e)
        {
            UI_IntChanged(tTubes_Row_1L.Text, x => Tube_Row_1L = x);
        }

        private void tTubes_Row_2L_TextChanged(object sender, EventArgs e)
        {
            UI_IntChanged(tTubes_Row_2L.Text, x => Tube_Row_2L = x);
        }
        private void tTubeOD_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubeOD.Text, x => TubeOD = x);
        }

        private void tTubeWallTHK_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubeWallTHK.Text, x => TubeWallTHK = x);
        }

        private void tFinOD_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tFinOD.Text, x => FinOD = x);
        }
        private void tFrontFinStripBack_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tFrontFinStripBack.Text, x => FinStripBack_Front = x);
        }

        private void tRearFinStripBack_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tRearFinStripBack.Text, x => FinStripBack_Rear = x);
        }
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
