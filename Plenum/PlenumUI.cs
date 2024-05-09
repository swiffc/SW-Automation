using ModelTools;
using Plenum.Floor;
using Plenum.Floor.Derived;
using Plenum.Walls;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using mTools = Tools.ModelTools;
using cTools = ModelTools.ReleaseCOM;
using Ftools = FileTools.FileTools;
using aTools = ModelTools.AssemblyTools;
using static FileTools.FileTools;
using Plenum.Stiffeners;
using static Plenum.Plenum;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;
using static FileTools.Properties.Settings;
using static FileTools.Base.Part;

namespace Plenum
{
    public partial class PlenumUI : Form
    {
        public PlenumUI()
        {
            InitializeComponent();

            SettingsChanged += UpdateUI;
        }

        private void UpdateUI()
        {
            isUserChange = false;

            // Job
            txt_JobNumber.Text = Default.Project;
            txt_JobCustomer.Text = Default.Customer;
            txt_JobClient.Text = Default.Client;
            txt_JobLocation.Text = Default.PlantLocation;
            txt_JobPO.Text = Default.PurchaseOrder;
            txt_JobItemNo.Text = Default.ItemNumber;
            txt_Initials.Text = Default.Initials;

            // Plenum
            txt_Length1.Text = Default.Plenum_Length.ToString();
            txt_Width1.Text = Default.Plenum_Width.ToString();
            txt_Depth1.Text = Default.Plenum_Depth.ToString();
            checkBox_MidCol.Checked = Default.Mid_Columns;
            EndcomboBox1.Text = Default.EndPanel_THK.ToString();
            SidecomboBox2.Text = Default.SidePanel_THK.ToString();

            // Beam
            comboBox_ColumnSize.Text = Default.Beam_Size;
            textBox_Depth.Text = Default.Beam_Depth.ToString();
            textBox_WebTHK.Text = Default.Beam_WebTHK.ToString();
            textBox_FlangeWidth.Text = Default.Beam_FlangeWidth.ToString();
            textBox_FlangeTHK.Text = Default.Beam_FlangeTHK.ToString();
            textBox_K.Text = Default.Beam_K.ToString();
            textBox_K1.Text = Default.Beam_K1.ToString();

            // Fan
            txt_FanCount1.Text = Default.Fan_Count.ToString();
            txt_FanDiameter.Text = Default.Fan_Diameter_Feet.ToString();
            txt_RingDepth.Text = Default.FanRing_Depth.ToString();

            checkBox1_MTRBeam.Checked = Default.MotorBeam_Required;

            comboBox1_driveDesign.Text = Default.MotorShaft_Orientation.ToString();

            textBox_ExtraLength.Text = Default.Johnson_ExtraLength < 1 ? "" : Default.Johnson_ExtraLength.ToString();
            textBox_SpliceOverride.Text = Default.FloorSplice_LengthOverride == 0 ? "" : Default.FloorSplice_LengthOverride.ToString();

            materialCombo.Text = Default.MaterialSpecSetting.ToString();

            txt_xShift.Text = FloorStiffener.XShiftAdjustment.ToString();
            lengthAdj.Text = FloorStiffener.LengthAdjustment.ToString();
            zShift_txt.Text = FloorStiffener.ZShiftAdjustment.ToString();

            checkBox2_dwg.Checked = Default.Toggle_CreateDrawing;
            checkBox3_save.Checked = Default.Toggle_Save;
            checkBox4_delete.Checked = Default.Toggle_DeleteFiles;

            textBox_Bank.Text = Default.Bank.ToString();
            comboBox_BraceType.Text = Default.BraceType.ToString();
            textBox_BraceAngle.Text = Default.BraceAngle.ToString();
            textBox_DriveWidth.Text = Default.MachineryMount_Width.ToString();
            textBoxShipBeamHeight.Text = Default.ShippingBeam_Height.ToString();
            textBox_ClipHeight.Text = Default.Clip_Height.ToString();

            textBox_TotalColumnHeight.Text = Default.TotalColumnHeight.ToString();
            mmHeight_Box.Text = Default.MachineryMount_Height.ToString();
            clipTHK_Box.Text = Default.Clip_THK.ToString();
            braceHoleDiameter_Box.Text = Default.HoleDiameter_Structural.ToString();
            flangeGageWT_Box.Text = WT_FlangeGage.ToString();

            textBox_EndOverride.Text = Default.EndStiffenerCount.ToString();
            textBox_DividerOverride.Text = Default.DividerStiffenerCount.ToString();

            isUserChange = true;
        }

        // Job
        private void txt_JobNumber_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(txt_JobNumber.Text, x => Default.Project = x);
        }
        private void txt_JobCustomer_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(txt_JobCustomer.Text, x => Default.Customer = x);
        }
        private void txt_JobClient_TextChanged_1(object sender, EventArgs e)
        {
            UI_StringChanged(txt_JobClient.Text, x => Default.Client = x);
        }
        private void txt_JobLocation_TextChanged_1(object sender, EventArgs e)
        {
            UI_StringChanged(txt_JobLocation.Text, x => Default.PlantLocation = x);
        }
        private void txt_JobPO_TextChanged_1(object sender, EventArgs e)
        {
            UI_StringChanged(txt_JobPO.Text, x => Default.PurchaseOrder = x);
        }
        private void txt_JobItemNo_TextChanged_1(object sender, EventArgs e)
        {
            UI_StringChanged(txt_JobItemNo.Text, x => Default.ItemNumber = x);
        }
        private void txt_Initials_TextChanged_1(object sender, EventArgs e)
        {
            UI_StringChanged(txt_Initials.Text, x => Default.Initials = x);
        }


        // Plenum
        private void txt_Length_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(txt_Length1.Text, x => Default.Plenum_Length = x);
        }

        private void txt_Width1_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(txt_Width1.Text, x => Default.Plenum_Width = x);
        }

        private void txt_Depth1_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(txt_Depth1.Text, x => Default.Plenum_Depth = x);
        }

        private void txt_FanCount1_TextChanged(object sender, EventArgs e)
        {
            UI_IntChanged(txt_FanCount1.Text, x => Default.Fan_Count = x);
        }

        private void checkBox_MidCol_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(checkBox_MidCol.Checked, value => Default.Mid_Columns = value);
        }


        // Column
        private bool isUserChange = true;

        private void textBox_Depth_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange)
            {
                UI_DoubleChanged(textBox_Depth.Text, x => Default.Beam_Depth = x);
                Default.Beam_Size = comboBox_ColumnSize.Text = "Custom";
            }
        }

        private void textBox_WebTHK_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange)
            {
                UI_DoubleChanged(textBox_WebTHK.Text, x => Default.Beam_WebTHK = x);
                Default.Beam_Size = comboBox_ColumnSize.Text = "Custom";
            }
        }

        private void textBox_FlangeWidth_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange)
            {
                UI_DoubleChanged(textBox_FlangeWidth.Text, x => Default.Beam_FlangeWidth = x);
                Default.Beam_Size = comboBox_ColumnSize.Text = "Custom";
            }
        }

        private void textBox_FlangeTHK_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange)
            {
                UI_DoubleChanged(textBox_FlangeTHK.Text, x => Default.Beam_FlangeTHK = x);
                Default.Beam_Size = comboBox_ColumnSize.Text = "Custom";
            }
        }

        private void textBox_K_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange)
            {
                UI_DoubleChanged(textBox_K.Text, x => Default.Beam_K = x);
                Default.Beam_Size = comboBox_ColumnSize.Text = "Custom";
            }
        }

        private void textBox_K1_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange)
            {
                UI_DoubleChanged(textBox_K1.Text, x => Default.Beam_K1 = x);
                Default.Beam_Size = comboBox_ColumnSize.Text = "Custom";
            }
        }



        // Refresh UI
        private void PlenumUI_Load(object sender, EventArgs e)
        {
            UpdateUI();
        }

        private void btn_Standard_Click(object sender, EventArgs e)
        {
            new Standard();
            Default.Beams_AreRotated = false;
            SaveSettings();
        }

        private void comboBox_ColumnSize_SelectedIndexChanged(object sender, EventArgs e)
        {
            UI_StringChanged(comboBox_ColumnSize.Text, x => Default.Beam_Size = x);
            if (Default.Beam_Size != "Custom")
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
        private void comboBox1_driveDesign_SelectedIndexChanged(object sender, EventArgs e)
        {
            UI_StringChanged(comboBox1_driveDesign.Text, x => Default.MotorShaft_Orientation = x);
        }

        private void btn_Johnson_Click(object sender, EventArgs e)
        {
            new Johnson();
            Default.Beams_AreRotated = true;
            SaveSettings();
        }

        private void btn_Legacy_Click(object sender, EventArgs e)
        {
            new Legacy();
            Default.Beams_AreRotated = true;
            SaveSettings();
        }

        private void textBox_ExtraLength_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(textBox_ExtraLength.Text, x => Default.Johnson_ExtraLength = x);
        }

        private void checkBox1_MTRBeam_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(checkBox1_MTRBeam.Checked, x => Default.MotorBeam_Required = x);
        }

        private void txt_FanDiameter_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(txt_FanDiameter.Text, x => Default.Fan_Diameter_Feet = x);
        }

        private void txt_RingDepth_TextChanged(object sender, EventArgs e)
        {
            double.TryParse(txt_RingDepth.Text, out double value);

            if (txt_RingDepth.Text.Length >= 2 ||
                value < 10 && value > 3)
            {
                UI_DoubleChanged(txt_RingDepth.Text, x => Ring_Depth = x);
            }
        }

        private void button1_save_Click(object sender, EventArgs e)
        {
            mTools.SaveEverything();
        }

        private void endComboBox1_SelectedIndexChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(EndcomboBox1.Text, x => Default.EndPanel_THK = x);
        }

        private void sideComboBox2_SelectedIndexChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(SidecomboBox2.Text, X => Default.SidePanel_THK = X);
        }

        private void materialCombo_SelectedIndexChanged(object sender, EventArgs e)
        {
            UI_StringChanged(materialCombo.Text, x => Default.MaterialSpecSetting = x);
        }

        private void xShift_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(txt_xShift.Text, out double xShift))
                FloorStiffener.XShiftAdjustment = xShift;
        }

        private void lengthAdj_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(lengthAdj.Text, out double length))
                FloorStiffener.LengthAdjustment = length;
        }

        private void zShift_txt_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(zShift_txt.Text, out double zShift))
                FloorStiffener.ZShiftAdjustment = zShift;
        }

        private void std_floor_Click(object sender, EventArgs e)
        {
            new Standard(Design.Standard);
        }

        private void jhn_floor_Click(object sender, EventArgs e)
        {
            new Johnson(Design.Johnson);
        }

        private void leg_floor_Click(object sender, EventArgs e)
        {
            new Legacy(Design.Legacy);
        }

        private void checkBox2_dwg_CheckedChanged(object sender, EventArgs e)
        {
            Default.Toggle_CreateDrawing = checkBox2_dwg.Checked;
            UI_BoolChanged(checkBox2_dwg.Checked, x => Default.Toggle_CreateDrawing = x);
        }

        private void checkBox3_save_CheckedChanged(object sender, EventArgs e)
        {
            Default.Toggle_Save = checkBox3_save.Checked;
            UI_BoolChanged(checkBox3_save.Checked, x => Default.Toggle_Save = x);
        }

        private void checkBox4_delete_CheckedChanged(object sender, EventArgs e)
        {
            Default.Toggle_DeleteFiles = checkBox4_delete.Checked;
            UI_BoolChanged(checkBox4_delete.Checked, x => Default.Toggle_DeleteFiles = x);
        }

        private void button1_Click(object sender, EventArgs e)
        {
            mTools.Unlock();
        }

        private void textBox_Bank_TextChanged(object sender, EventArgs e)
        {
            UI_CharChanged(textBox_Bank.Text, x => Default.Bank = x);
        }

        private void comboBox_BraceType_SelectedIndexChanged(object sender, EventArgs e)
        {
            UI_StringChanged(comboBox_BraceType.Text, x => Default.BraceType = x);
        }

        private void textBox_BraceAngle_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(textBox_BraceAngle.Text, x => Default.BraceAngle = x);
        }

        private void textBox_DriveWidth_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(textBox_DriveWidth.Text, x => Default.MachineryMount_Width = x);
        }

        private void textBoxShipBeamHeight_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(textBoxShipBeamHeight.Text, x => Default.ShippingBeam_Height = x);
        }

        private void label23_Click(object sender, EventArgs e)
        {

        }

        private void label3_Click(object sender, EventArgs e)
        {

        }

        private void textBox_ClipHeight_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(textBox_ClipHeight.Text, x => Default.Clip_Height = x);

        }

        private void textBox_TotalColumnHeight_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(textBox_TotalColumnHeight.Text, x => Default.TotalColumnHeight = x);
        }

        private void mmHeight_Box_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(mmHeight_Box.Text, x => Default.MachineryMount_Height = x);
        }

        private void clipTHK_Box_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(clipTHK_Box.Text, x => Default.Clip_THK = x);
        }

        private void braceHoleDiameter_Box_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(braceHoleDiameter_Box.Text, x => Default.HoleDiameter_Structural = x);
        }

        private void flangeGageWT_Box_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(flangeGageWT_Box.Text, x => Default.WT_FlangeGage = x);
        }

        private void textBox_SpliceOverride_TextChanged(object sender, EventArgs e)
        {
            bool saveIsEnabled = true;
            if (textBox_SpliceOverride.Text.EndsWith(".") ||
                textBox_SpliceOverride.Text.EndsWith(".0"))
            {
                saveIsEnabled = false;
            }

            if (double.TryParse(textBox_SpliceOverride.Text, out double splice))
                Default.FloorSplice_LengthOverride = splice;
            else
                Default.FloorSplice_LengthOverride = 0;

            if (saveIsEnabled)
                SaveSettings();
        }

        private void PlenumUI_FormClosing(object sender, FormClosingEventArgs e)
        {
            SaveSettings();
        }

        private void textBox_EndOverride_TextChanged(object sender, EventArgs e)
        {
            UI_IntChanged(textBox_EndOverride.Text, x => Default.EndStiffenerCount = x);
        }

        private void textBox_SideOverride_TextChanged(object sender, EventArgs e)
        {
            UI_IntChanged(textBox_DividerOverride.Text, x => Default.DividerStiffenerCount = x);
        }
    }
}
