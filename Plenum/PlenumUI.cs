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
        }

        // Job
        private void txt_JobNumber_TextChanged(object sender, EventArgs e)
        {
            JobInfo.Project = txt_JobNumber.Text;
        }
        private void txt_JobCustomer_TextChanged(object sender, EventArgs e)
        {
            JobInfo.Customer = txt_JobCustomer.Text;
        }
        private void txt_JobClient_TextChanged_1(object sender, EventArgs e)
        {
            JobInfo.Client = txt_JobClient.Text;
        }
        private void txt_JobLocation_TextChanged_1(object sender, EventArgs e)
        {
            JobInfo.PlantLocation = txt_JobLocation.Text;
        }
        private void txt_JobPO_TextChanged_1(object sender, EventArgs e)
        {
            JobInfo.PurchaseOrder = txt_JobPO.Text;
        }
        private void txt_JobItemNo_TextChanged_1(object sender, EventArgs e)
        {
            JobInfo.ItemNumber = txt_JobItemNo.Text;
        }
        private void txt_Initials_TextChanged_1(object sender, EventArgs e)
        {
            JobInfo.Initials = txt_Initials.Text;
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
                Default.Save();
            }
        }

        private void textBox_WebTHK_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange)
            {
                UI_DoubleChanged(textBox_WebTHK.Text, x => Default.Beam_WebTHK = x);
                Default.Beam_Size = comboBox_ColumnSize.Text = "Custom";
                Default.Save();
            }
        }

        private void textBox_FlangeWidth_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange)
            {
                UI_DoubleChanged(textBox_FlangeWidth.Text, x => Default.Beam_FlangeWidth = x);
                Default.Beam_Size = comboBox_ColumnSize.Text = "Custom";
                Default.Save();
            }
        }

        private void textBox_FlangeTHK_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange)
            {
                UI_DoubleChanged(textBox_FlangeTHK.Text, x => Default.Beam_FlangeTHK = x);
                Default.Beam_Size = comboBox_ColumnSize.Text = "Custom";
                Default.Save();
            }
        }

        private void textBox_K_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange)
            {
                UI_DoubleChanged(textBox_K.Text, x => Default.Beam_K = x);
                Default.Beam_Size = comboBox_ColumnSize.Text = "Custom";
                Default.Save();
            }
        }

        private void textBox_K1_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange)
            {
                UI_DoubleChanged(textBox_K1.Text, x => Default.Beam_K1 = x);
                Default.Beam_Size = comboBox_ColumnSize.Text = "Custom";
                Default.Save();
            }
        }



        // Refresh UI
        private void PlenumUI_Load(object sender, EventArgs e)
        {
            // Job
            txt_JobNumber.Text = JobInfo.Project;
            txt_JobCustomer.Text = JobInfo.Customer;
            txt_JobClient.Text = JobInfo.Client;
            txt_JobLocation.Text = JobInfo.PlantLocation;
            txt_JobPO.Text = JobInfo.PurchaseOrder;
            txt_JobItemNo.Text = JobInfo.ItemNumber;
            txt_Initials.Text = JobInfo.Initials;

            // Plenum
            txt_Length1.Text = Default.Plenum_Length.ToString();
            txt_Width1.Text = Default.Plenum_Width.ToString();
            txt_Depth1.Text = Default.Plenum_Depth.ToString();
            checkBox_MidCol.Checked = Default.Mid_Columns;
            EndcomboBox1.Text = EndPanel_THK.ToString();
            SidecomboBox2.Text = SidePanel_THK.ToString();

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

            checkBox1_MTRBeam.Checked = MotorBeamWld.Enabled;
            textBox1_columnLength.Text = PlenumColumn.Height.ToString();

            comboBox1_driveDesign.Text = Default.MotorShaft_Orientation.ToString();

            textBox_ExtraLength.Text = Johnson._extraLength == 0 ? "" : Johnson._extraLength.ToString();

            materialCombo.Text = Default.MaterialSpecSetting.ToString();

            txt_xShift.Text = FloorStiffener.XShiftAdjustment.ToString();
            lengthAdj.Text = FloorStiffener.LengthAdjustment.ToString();
            zShift_txt.Text = FloorStiffener.ZShiftAdjustment.ToString();

            checkBox1_locations.Checked = ToggleRelocate;
            checkBox2_dwg.Checked = CommonData.ToggleCreateDrawing;
            checkBox3_save.Checked = CommonData.ToggleSave;
            checkBox4_delete.Checked = CommonData.ToggleDeleteFiles;
        }

        private void btn_Standard_Click(object sender, EventArgs e)
        {
            new Standard();
        }

        private void comboBox_ColumnSize_SelectedIndexChanged(object sender, EventArgs e)
        {
            isUserChange = false;

            UI_StringChanged(comboBox_ColumnSize.Text, x => Default.Beam_Size = x);
            if (Default.Beam_Size != "Custom")
            {
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

                Default.Save();
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
        }

        private void btn_Legacy_Click(object sender, EventArgs e)
        {
            new Legacy();
        }

        private void textBox_ExtraLength_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(textBox_ExtraLength.Text, out double extraLength))
                Johnson.ExtraLength = extraLength;
        }

        private void checkBox1_MTRBeam_CheckedChanged(object sender, EventArgs e)
        {
            MotorBeamWld.Enabled = checkBox1_MTRBeam.Checked;
        }

        private void txt_FanDiameter_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(txt_FanDiameter.Text, x => Default.Fan_Diameter_Feet = x);
        }

        private void txt_RingDepth_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(txt_RingDepth.Text, x => Default.FanRing_Depth = x);
        }

        private void textBox1_columnLength_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(textBox1_columnLength.Text, out double columnLength))
                PlenumColumn.Height = columnLength;
        }

        private void button1_save_Click(object sender, EventArgs e)
        {
            mTools.SaveEverything();
        }

        private void endComboBox1_SelectedIndexChanged(object sender, EventArgs e)
        {
            switch (EndcomboBox1.Text)
            {
                case "0.1344":
                    Default.EndPanel_THK = 0.1344;
                    break;
                case "0.1875":
                    Default.EndPanel_THK = 0.1875;
                    break;
                case "0.2500":
                    Default.EndPanel_THK = 0.2500;
                    break;
                case "0.3125":
                    Default.EndPanel_THK = 0.3125;
                    break;
                case "0.3750":
                    Default.EndPanel_THK = 0.3750;
                    break;
            }
        }

        private void sideComboBox2_SelectedIndexChanged(object sender, EventArgs e)
        {
            switch (SidecomboBox2.Text)
            {
                case "0.1344":
                    Default.SidePanel_THK = 0.1344;
                    break;
                case "0.1875":
                    Default.SidePanel_THK = 0.1875;
                    break;
                case "0.2500":
                    Default.SidePanel_THK = 0.2500;
                    break;
                case "0.3125":
                    Default.SidePanel_THK = 0.3125;
                    break;
                case "0.3750":
                    Default.SidePanel_THK = 0.3750;
                    break;
            }
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

        private void checkBox1_locations_CheckedChanged(object sender, EventArgs e)
        {
            ToggleRelocate = checkBox1_locations.Checked;
        }

        private void checkBox2_dwg_CheckedChanged(object sender, EventArgs e)
        {
            CommonData.ToggleCreateDrawing = checkBox2_dwg.Checked;
        }

        private void checkBox3_save_CheckedChanged(object sender, EventArgs e)
        {
            CommonData.ToggleSave = checkBox3_save.Checked;
        }

        private void checkBox4_delete_CheckedChanged(object sender, EventArgs e)
        {
            CommonData.ToggleDeleteFiles = checkBox4_delete.Checked;
        }

        private void button1_Click(object sender, EventArgs e)
        {
            mTools.Unlock();
        }


    }
}
