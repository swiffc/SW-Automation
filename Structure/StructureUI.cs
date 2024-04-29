using FileTools;
using Structure.Braces;
using Structure.Columns.Derived.Children;
using System;
using System.Windows.Forms;
using static Tools.ModelTools;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;
using static FileTools.Properties.Settings;

namespace Structure
{
    public partial class StructureUI : Form
    {
        public StructureUI()
        {
            InitializeComponent();

            CommonData.OnFanCountChanged += SharedProperties_OnFanCountChanged;

            SettingsChanged += UpdateUI;
        }
        #region Events

        private DataGridView dataGridView;
        private void SharedProperties_OnFanCountChanged()
        {
            this.Invoke((MethodInvoker)delegate
            {
                midColumns_Box.Checked = CommonData.Mid_Columns;
            });
        }
        private bool isUserChange = true;

        #endregion
        #region Buttons

        private void btn_Standard_Click(object sender, EventArgs e)
        {
            new Structure(25, "Structure");
        }
        private void button1_Click(object sender, EventArgs e)
        {
            new Structure(25, "Structure");
        }
        private void button1_save_Click(object sender, EventArgs e)
        {
            SaveEverything();
        }

        #endregion
        private void UpdateUI()
        {
            isUserChange = false;

            #region Job_Load

            job_Box.Text = Default.Project;
            customer_Box.Text = Default.Customer;
            client_Box.Text = Default.Client;
            location_Box.Text = Default.PlantLocation;
            purchaseOrder_Box.Text = Default.PurchaseOrder;
            itemNumber_Box.Text = Default.ItemNumber;
            initials_Box.Text = Default.Initials;

            #endregion
            #region Columns_Load

            width_TextBox.Text = Default.Plenum_Width.ToString();
            length_TextBox.Text = Default.Plenum_Length.ToString();
            height_TextBox.Text = Default.TotalColumnHeight.ToString();
            midColumns_Box.Checked = Default.Mid_Columns;
            rotate_Box.Checked = Default.Beams_AreRotated;
            beamSize_Box.Text = Default.Beam_Size;
            textBox_Depth.Text = Default.Beam_Depth.ToString();
            textBox_WebTHK.Text = Default.Beam_WebTHK.ToString();
            textBox_FlangeWidth.Text = Default.Beam_FlangeWidth.ToString();
            textBox_FlangeTHK.Text = Default.Beam_FlangeTHK.ToString();
            textBox_K.Text = Default.Beam_K.ToString();
            textBox_K1.Text = Default.Beam_K1.ToString();

            #endregion
            #region BasePlate_Load

            bpWidth_Box.Text = Default.BasePlate_Width.ToString();
            bpLength_Box.Text = Default.BasePlate_Length.ToString();
            wSPA_Box.Text = Default.BasePlate_WidthHoleSpacing.ToString();
            lSPA_Box.Text = Default.BasePlate_LengthHoleSpacing.ToString();
            dia_Box.Text = Default.BasePlate_HoleDiameter.ToString();

            #endregion
            #region Plenum_Load

            fanCount_Box.Text = Default.Fan_Count.ToString();
            depth_Box.Text = Default.Plenum_Depth.ToString();

            #endregion
            #region MachineryMount_Load

            mmHeight_Box.Text = Default.MachineryMount_Height.ToString();

            #endregion
            #region Brace_Load

            // General
            braceType_Box.Text = Default.BraceType;
            clipTHK_Box.Text = Default.Clip_THK.ToString();
            braceHoleDiameter_Box.Text = Default.HoleDiameter_Structural.ToString();
            braceAngle_Box.Text = Default.BraceAngle.ToString();

            #endregion
            #region Toggles_Load

            createDrawing_Toggle.Checked = Default.Toggle_CreateDrawing;
            save_Toggle.Checked = Default.Toggle_Save;
            delete_Toggle.Checked = Default.Toggle_DeleteFiles;

            #endregion

            isUserChange = true;
        }
        private void StructureUI_Load(object sender, EventArgs e)
        {
            UpdateUI();
        }
        #region Job_Changed

        private void job_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(job_Box.Text, x => Default.Project = x);
        }

        private void customer_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(customer_Box.Text, x => Default.Customer = x);
        }

        private void client_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(client_Box.Text, x => Default.Client = x);
        }

        private void location_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(location_Box.Text, x => Default.PlantLocation = x);
        }

        private void purchaseOrder_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(purchaseOrder_Box.Text, x => Default.PurchaseOrder = x);
        }

        private void itemNumber_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(itemNumber_Box.Text, x => Default.ItemNumber = x);
        }

        private void initials_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(initials_Box.Text, x => Default.Initials = x);
        }



        #endregion
        #region Columns_Changed

        private void width_TextBox_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(width_TextBox.Text, x => Default.Plenum_Width = x);
        }
        private void length_TextBox_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(length_TextBox.Text, x => Default.Plenum_Length = x);
        }
        private void height_TextBox_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(height_TextBox.Text, x => Default.TotalColumnHeight = x);
        }
        private void midColumn_Box_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(midColumns_Box.Checked, x => Default.Mid_Columns = x);
        }
        private void rotate_Box_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(rotate_Box.Checked, x => Default.Beams_AreRotated = x);
        }
        private void beamSize_Box_SelectedIndexChanged(object sender, EventArgs e)
        {
            UI_StringChanged(beamSize_Box.Text, x => Default.Beam_Size = x);
            if (Beam_Size != "Custom")
            {
                isUserChange = false;

                Default.Beam_Depth = Beam_Depth;
                textBox_Depth.Text = Default.Beam_Depth.ToString();

                Default.Beam_WebTHK = Beam_WebTHK;
                textBox_WebTHK.Text = Default.Beam_WebTHK.ToString();

                Default.Beam_FlangeWidth = Beam_FlangeWidth;
                textBox_FlangeWidth.Text = Default.Beam_FlangeWidth.ToString();

                Default.Beam_FlangeTHK = Beam_FlangeTHK;
                textBox_FlangeTHK.Text = Default.Beam_FlangeTHK.ToString();

                Default.Beam_K = Beam_K;
                textBox_K.Text = Default.Beam_K.ToString();

                Default.Beam_K1 = Beam_K1;
                textBox_K1.Text = Default.Beam_K1.ToString();
            }

            isUserChange = true;
        }
        private void textBox_Depth_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange)
            {
                UI_DoubleChanged(textBox_Depth.Text, x => Default.Beam_Depth = x);
                Default.Beam_Size = beamSize_Box.Text = "Custom";
            }
            
        }

        private void textBox_WebTHK_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange)
            {
                UI_DoubleChanged(textBox_WebTHK.Text, x => Default.Beam_WebTHK = x);
                Default.Beam_Size = beamSize_Box.Text = "Custom";
            }
        }

        private void textBox_FlangeWidth_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange)
            {
                UI_DoubleChanged(textBox_FlangeWidth.Text, x => Default.Beam_FlangeWidth = x);
                Default.Beam_Size = beamSize_Box.Text = "Custom";
            }
        }

        private void textBox_FlangeTHK_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange)
            {
                UI_DoubleChanged(textBox_FlangeTHK.Text, x => Default.Beam_FlangeTHK = x);
                Default.Beam_Size = beamSize_Box.Text = "Custom";
            }
        }

        private void textBox_K_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange)
            {
                UI_DoubleChanged(textBox_K.Text, x => Default.Beam_K = x);
                Default.Beam_Size = beamSize_Box.Text = "Custom";
            }
        }

        private void textBox_K1_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange)
            {
                UI_DoubleChanged(textBox_K1.Text, x => Default.Beam_K1 = x);
                Default.Beam_Size = beamSize_Box.Text = "Custom";
            }
        }

        #endregion
        #region BasePlate_Changed

        private void bpWidth_Box_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(bpWidth_Box.Text, x => Default.BasePlate_Width = x);
        }

        private void bpLength_Box_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(bpLength_Box.Text, x => Default.BasePlate_Length = x);
        }

        private void wSPA_Box_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(wSPA_Box.Text, x => Default.BasePlate_WidthHoleSpacing = x);
        }

        private void lSPA_Box_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(lSPA_Box.Text, x => Default.BasePlate_LengthHoleSpacing = x);
        }

        private void dia_Box_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(dia_Box.Text, x => Default.BasePlate_HoleDiameter = x);
        }

        #endregion
        #region Plenum_Changed

        private void fanCount_Box_TextChanged(object sender, EventArgs e)
        {
            UI_IntChanged(fanCount_Box.Text, x => Default.Fan_Count = x);
        }
        private void depth_Box_TextChanged(object sender, EventArgs e)
        {
            UI_IntChanged(depth_Box.Text, x => Default.Plenum_Depth = x);
        }


        #endregion
        #region MachineryMount_Changed

        private void mmHeight_Box_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(mmHeight_Box.Text, x => Default.MachineryMount_Height = x);
        }


        #endregion
        #region Braced_Changed

        // General
        private void braceType_Box_SelectedIndexChanged_1(object sender, EventArgs e)
        {
            UI_StringChanged(braceType_Box.Text, x => Default.BraceType = x);
            TextBox[] wtBoxes = { depthWT_Box, stemTHKWT_Box, flangeWidthWT_Box, flangeTHKWT_Box, kWT_Box, k1WT_Box, flangeGageWT_Box };
            TextBox[] lBoxes = { leg1_Box, leg2_Box, gage_Box, thkL_Box, kL_Box };

            // A method to clear and set read-only state for a collection of text boxes
            void SetTextBoxesState(TextBox[] textBoxes, bool readOnly, string text = "")
            {
                foreach (var textBox in textBoxes)
                {
                    textBox.ReadOnly = readOnly;
                    textBox.Text = text;
                }
            }
            void PopulateL()
            {
                leg1_Box.ReadOnly = false; leg1_Box.Text = Default.AngleBrace_Leg1.ToString();
                leg2_Box.ReadOnly = false; leg2_Box.Text = Default.AngleBrace_Leg2.ToString();
                gage_Box.ReadOnly = false; gage_Box.Text = Default.AngleBrace_Gage.ToString();
                thkL_Box.ReadOnly = false; thkL_Box.Text = Default.AngleBrace_THK.ToString();
                kL_Box.ReadOnly = false; kL_Box.Text = Default.AngleBrace_K.ToString();
            }
            void PopulateWT()
            {
                depthWT_Box.ReadOnly = false; depthWT_Box.Text = WT_Depth.ToString();
                stemTHKWT_Box.ReadOnly = false; stemTHKWT_Box.Text = WT_StemTHK.ToString();
                flangeWidthWT_Box.ReadOnly = false; flangeWidthWT_Box.Text = WT_FlangeWidth.ToString();
                flangeTHKWT_Box.ReadOnly = false; flangeTHKWT_Box.Text = WT_FlangeTHK.ToString();
                kWT_Box.ReadOnly = false; kWT_Box.Text = WT_K.ToString();
                k1WT_Box.ReadOnly = false; k1WT_Box.Text = WT_K1.ToString();
                flangeGageWT_Box.ReadOnly = false; flangeGageWT_Box.Text = WT_FlangeGage.ToString();
            }

            switch (braceType_Box.Text)
            {
                case "L":
                case "LL":
                case "X":
                    SetTextBoxesState(wtBoxes, true);
                    PopulateL();
                    break;

                case "T":
                    SetTextBoxesState(lBoxes, true);
                    PopulateWT();
                    break;

                default:
                    SetTextBoxesState(wtBoxes, false); 
                    SetTextBoxesState(lBoxes, false);
                    PopulateL();
                    PopulateWT();
                    break;
            }
        }
        private void clipTHK_Box_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(clipTHK_Box.Text, x => Default.Clip_THK = x);    
        }
        private void braceHoleDiameter_Box_TextChanged_1(object sender, EventArgs e)
        {
            UI_DoubleChanged(braceHoleDiameter_Box.Text, x => Default.HoleDiameter_Structural = x);
        }
        private void braceAngle_Box_TextChanged_1(object sender, EventArgs e)
        {
            UI_DoubleChanged(braceAngle_Box.Text, x => Default.BraceAngle = x);
        }


        // L
        private void leg1_Box_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(leg1_Box.Text, x => Default.AngleBrace_Leg1 = x);
        }
        private void leg2_Box_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(leg2_Box.Text, x => Default.AngleBrace_Leg2 = x);
        }
        private void gage_Box_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(gage_Box.Text, x => Default.AngleBrace_Gage = x);
        }
        private void thk_Box_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(thkL_Box.Text, x => Default.AngleBrace_THK = x);
        }
        private void kL_Box_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(kL_Box.Text, x => Default.AngleBrace_K = x);
        }


        // WT
        private void depthWT_Box_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(depthWT_Box.Text, x => Default.WT_Depth = x);
        }
        private void stemTHKWT_Box_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(stemTHKWT_Box.Text, x => Default.WT_StemTHK = x);
        }
        private void flangeWidthWT_Box_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(flangeWidthWT_Box.Text, x => Default.WT_FlangeWidth = x);
        }
        private void flangeTHKWT_Box_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(flangeTHKWT_Box.Text, x => Default.WT_FlangeTHK = x);
        }
        private void kWT_Box_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(kWT_Box.Text, x => Default.WT_K = x);
        }
        private void k1WT_Box_TextChanged(object sender, EventArgs e)
        {

            UI_DoubleChanged(k1WT_Box.Text, x => Default.WT_K1 = x);
        }
        private void flangeGageWT_Box_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(flangeGageWT_Box.Text, x => Default.WT_FlangeGage = x);
        }

        #endregion
        #region Toggles_Changed

        private void checkBox2_dwg_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(createDrawing_Toggle.Checked, x => Default.Toggle_CreateDrawing = x);
        }

        private void checkBox3_save_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(save_Toggle.Checked, x => Default.Toggle_Save = x);
        }

        private void checkBox4_delete_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(delete_Toggle.Checked, x => Default.Toggle_DeleteFiles = x);
        }

        #endregion


    }
}
