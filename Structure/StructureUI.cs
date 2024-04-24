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
        }
        #region CustomEvents

        private DataGridView dataGridView;
        private void SharedProperties_OnFanCountChanged()
        {
            this.Invoke((MethodInvoker)delegate
            {
                midColumns_Box.Checked = CommonData.MidColumns;
            });
        }

        #endregion
        #region Buttons

        private void btn_Standard_Click(object sender, EventArgs e)
        {
            Default.PlenumDesignSetting = Design.Standard.ToString();
            Default.Beams_AreRotated = false;
            Default.Save();

            new Structure(25, "Structure");
            new Plenum.Standard();
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
        private void StructureUI_Load(object sender, EventArgs e)
        {
            #region Job_Load

            job_Box.Text = StaticFileTools.Project;
            customer_Box.Text = StaticFileTools.Customer;
            client_Box.Text = StaticFileTools.Client;
            location_Box.Text = StaticFileTools.Location;
            purchaseOrder_Box.Text = StaticFileTools.PurchaseOrder;
            itemNumber_Box.Text = StaticFileTools.ItemNumber;
            initials_Box.Text = StaticFileTools.Initials;

            #endregion
            #region Columns_Load

            width_TextBox.Text = CommonData.Width.ToString();
            length_TextBox.Text = CommonData.Length.ToString();
            height_TextBox.Text = CommonData.TotalColumnHeight.ToString();
            midColumns_Box.Checked = CommonData.MidColumns;
            rotate_Box.Checked = Default.Beams_AreRotated;
            beamSize_Box.Text = Beam_Size;
            textBox_Depth.Text = Beam_Depth.ToString();
            textBox_WebTHK.Text = Beam_WebTHK.ToString();
            textBox_FlangeWidth.Text = Beam_FlangeWidth.ToString();
            textBox_FlangeTHK.Text = Beam_FlangeTHK.ToString();
            textBox_K.Text = Beam_K.ToString();
            textBox_K1.Text = Beam_K1.ToString();

            #endregion
            #region BasePlate_Load

            bpWidth_Box.Text = BasePlate.LocalWidth.ToString();
            bpLength_Box.Text = BasePlate.LocalLength.ToString();
            wSPA_Box.Text = BasePlate.WidthHoleSpacing.ToString();
            lSPA_Box.Text = BasePlate.LengthHoleSpacing.ToString();
            dia_Box.Text = BasePlate.HoleDiameter.ToString();

            #endregion
            #region Plenum_Load

            fanCount_Box.Text = CommonData.FanCount.ToString();
            depth_Box.Text = CommonData.PlenumDepth.ToString();

            #endregion
            #region MachineryMount_Load

            mmHeight_Box.Text = CommonData.MachineryMountHeight.ToString();

            #endregion
            #region Brace_Load

            // General
            braceType_Box.Text = CommonData.BraceType;
            clipTHK_Box.Text = Default.Clip_THK.ToString();
            braceHoleDiameter_Box.Text = HoleDiameter_Structural.ToString();
            braceAngle_Box.Text = CommonData.BraceAngle.ToString();

            #endregion
            #region Toggles_Load

            createDrawing_Toggle.Checked = ToggleCreateDrawing;
            save_Toggle.Checked = ToggleSave;
            delete_Toggle.Checked = ToggleDeleteFiles;

            #endregion
        }
        #region Job_Changed

        private void txt_JobNumber_TextChanged(object sender, EventArgs e)
        {
            StaticFileTools.Project = job_Box.Text;
        }
        private void txt_JobCustomer_TextChanged(object sender, EventArgs e)
        {
            StaticFileTools.Customer = customer_Box.Text;
        }
        private void txt_JobClient_TextChanged(object sender, EventArgs e)
        {
            StaticFileTools.Client = client_Box.Text;
        }
        private void txt_JobLocation_TextChanged(object sender, EventArgs e)
        {
            StaticFileTools.Location = location_Box.Text;
        }
        private void txt_JobPO_TextChanged(object sender, EventArgs e)
        {
            StaticFileTools.PurchaseOrder = purchaseOrder_Box.Text;
        }
        private void txt_JobItemNo_TextChanged(object sender, EventArgs e)
        {
            StaticFileTools.ItemNumber = itemNumber_Box.Text;
        }
        private void txt_Initials_TextChanged(object sender, EventArgs e)
        {
            StaticFileTools.Initials = initials_Box.Text;
        }



        #endregion
        #region Columns_Changed

        private void width_TextBox_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(width_TextBox.Text, out double value))
                CommonData.Width = value;
        }

        private void length_TextBox_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(length_TextBox.Text, out double value))
                CommonData.Length = value;
        }

        private void height_TextBox_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(height_TextBox.Text, out double value))
                CommonData.TotalColumnHeight = value;
        }

        private void midColumn_Box_CheckedChanged(object sender, EventArgs e)
        {
            CommonData.MidColumns = midColumns_Box.Checked;
        }
        private void rotate_Box_CheckedChanged(object sender, EventArgs e)
        {
            Default.Beams_AreRotated = rotate_Box.Checked;
            Default.Save();
        }
        private bool isUserChange = true;
        private void beamSize_Box_SelectedIndexChanged(object sender, EventArgs e)
        {
            isUserChange = false;

            Default.Beam_Size = beamSize_Box.Text;
            if (Beam_Size != "Custom")
            {
                ResetSize();
                textBox_Depth.Text = Beam_Depth.ToString(); 
                textBox_WebTHK.Text = Beam_WebTHK.ToString();
                textBox_FlangeWidth.Text = Beam_FlangeWidth.ToString();
                textBox_FlangeTHK.Text = Beam_FlangeTHK.ToString();
                textBox_K.Text = Beam_K.ToString();
                textBox_K1.Text = Beam_K1.ToString();
            }

            isUserChange = true;
        }
        private void textBox_Depth_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange && double.TryParse(textBox_Depth.Text, out double depth))
            {
                Beam_Depth = depth;
                Default.Beam_Size = beamSize_Box.Text = "Custom";
            }
        }

        private void textBox_WebTHK_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange && double.TryParse(textBox_WebTHK.Text, out double webTHK))
            {
                Beam_WebTHK = webTHK;
                Default.Beam_Size = beamSize_Box.Text = "Custom";
            }
        }

        private void textBox_FlangeWidth_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange && double.TryParse(textBox_FlangeWidth.Text, out double flangeWidth))
            {
                Beam_FlangeWidth = flangeWidth;
                Default.Beam_Size = beamSize_Box.Text = "Custom";
            }
        }

        private void textBox_FlangeTHK_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange && double.TryParse(textBox_FlangeTHK.Text, out double flangeTHK))
            {
                Beam_FlangeTHK = flangeTHK;
                Default.Beam_Size = beamSize_Box.Text = "Custom";
            }
        }

        private void textBox_K_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange && double.TryParse(textBox_K.Text, out double k))
            {
                Beam_K = k;
                Default.Beam_Size = beamSize_Box.Text = "Custom";
            }
        }

        private void textBox_K1_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange && double.TryParse(textBox_K1.Text, out double k1))
            {
                Beam_K1 = k1;
                Default.Beam_Size = beamSize_Box.Text = "Custom";
            }
        }

        #endregion
        #region BasePlate_Changed

        private void bpWidth_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(bpWidth_Box.Text, out double value))
                BasePlate.LocalWidth = value;
        }

        private void bpLength_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(bpLength_Box.Text, out double value))
                BasePlate.LocalLength = value;
        }

        private void wSPA_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(wSPA_Box.Text, out double value))
                BasePlate.WidthHoleSpacing = value;
        }

        private void lSPA_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(lSPA_Box.Text, out double value))
                BasePlate.LengthHoleSpacing = value;
        }

        private void dia_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(dia_Box.Text, out double value))
                BasePlate.HoleDiameter = value;
        }

        #endregion
        #region Plenum_Changed

        private void fanCount_Box_TextChanged(object sender, EventArgs e)
        {
            if (int.TryParse(fanCount_Box.Text, out int value))
                CommonData.FanCount = value;
        }
        private void depth_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(depth_Box.Text, out double value))
                CommonData.PlenumDepth = value;
        }


        #endregion
        #region MachineryMount_Changed

        private void mmHeight_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(mmHeight_Box.Text, out double value))
                CommonData.MachineryMountHeight = value;
        }


        #endregion
        #region Braced_Changed

        // General
        private void braceType_Box_SelectedIndexChanged_1(object sender, EventArgs e)
        {
            CommonData.BraceType = braceType_Box.Text;
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
                leg1_Box.ReadOnly = false; leg1_Box.Text = AngleBrace.Leg1.ToString();
                leg2_Box.ReadOnly = false; leg2_Box.Text = AngleBrace.Leg2.ToString();
                gage_Box.ReadOnly = false; gage_Box.Text = AngleBrace.Gage.ToString();
                thkL_Box.ReadOnly = false; thkL_Box.Text = AngleBrace.THK.ToString();
                kL_Box.ReadOnly = false; kL_Box.Text = AngleBrace.K.ToString();
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
            if (double.TryParse(clipTHK_Box.Text, out double value))
            {
                Default.Clip_THK = value;
                Default.Save();
            }
                
        }
        private void braceHoleDiameter_Box_TextChanged_1(object sender, EventArgs e)
        {
            if (double.TryParse(braceHoleDiameter_Box.Text, out double value))
                HoleDiameter_Structural = value;
        }
        private void braceAngle_Box_TextChanged_1(object sender, EventArgs e)
        {
            if (double.TryParse(braceAngle_Box.Text, out double value))
                CommonData.BraceAngle = value;
        }


        // L
        private void leg1_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(leg1_Box.Text, out double value))
                AngleBrace.Leg1 = value;
        }
        private void leg2_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(leg2_Box.Text, out double value))
                AngleBrace.Leg2 = value;
        }
        private void gage_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(gage_Box.Text, out double value))
                AngleBrace.Gage = value;
        }
        private void thk_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(thkL_Box.Text, out double value))
                AngleBrace.THK = value;

        }
        private void kL_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(kL_Box.Text, out double value))
                AngleBrace.K = value;
        }


        // WT
        private void depthWT_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(depthWT_Box.Text, out double value))
                WT_Depth = value;
        }
        private void stemTHKWT_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(stemTHKWT_Box.Text, out double value))
                WT_StemTHK = value;
        }
        private void flangeWidthWT_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(flangeWidthWT_Box.Text, out double value))
                WT_FlangeWidth = value;
        }
        private void flangeTHKWT_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(flangeTHKWT_Box.Text, out double value))
                WT_FlangeTHK = value;
        }
        private void kWT_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(kWT_Box.Text, out double value))
                WT_K = value;
        }
        private void k1WT_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(k1WT_Box.Text, out double value))
                WT_K1 = value;
        }
        private void flangeGageWT_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(flangeGageWT_Box.Text, out double value))
                WT_FlangeGage = value;
        }

        #endregion
        #region Toggles_Changed

        private void checkBox2_dwg_CheckedChanged(object sender, EventArgs e)
        {
            ToggleCreateDrawing = createDrawing_Toggle.Checked;
        }

        private void checkBox3_save_CheckedChanged(object sender, EventArgs e)
        {
            ToggleSave = save_Toggle.Checked;
        }

        private void checkBox4_delete_CheckedChanged(object sender, EventArgs e)
        {
            ToggleDeleteFiles = delete_Toggle.Checked;
        }

        #endregion
    }
}
