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

namespace Plenum
{
    public partial class PlenumUI : Form
    {
        public PlenumUI()
        {
            InitializeComponent();
        }

        // Job
        private void txt_JobNumber_TextChanged_1(object sender, EventArgs e)
        {
            JobInfo.Project = txt_JobNumber.Text;
        }
        private void txt_JobCustomer_TextChanged_1(object sender, EventArgs e)
        {
            JobInfo.Customer = txt_JobCustomer.Text;
        }
        private void txt_JobClient_TextChanged(object sender, EventArgs e)
        {
            JobInfo.Client = txt_JobClient.Text;
        }
        private void txt_JobLocation_TextChanged(object sender, EventArgs e)
        {
            JobInfo.PlantLocation = txt_JobLocation.Text;
        }
        private void txt_JobPO_TextChanged(object sender, EventArgs e)
        {
            JobInfo.PurchaseOrder = txt_JobPO.Text;
        }
        private void txt_JobItemNo_TextChanged(object sender, EventArgs e)
        {
            JobInfo.ItemNumber = txt_JobItemNo.Text;
        }
        private void txt_Initials_TextChanged(object sender, EventArgs e)
        {
            JobInfo.Initials = txt_Initials.Text;
        }


        // Plenum
        private void txt_Length_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(txt_Length1.Text, out double length))
            {
                Plenum.Length = length;
            }
        }

        private void txt_Width1_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(txt_Width1.Text, out double width))
            {
                Plenum.Width = width;
            }
        }

        private void txt_Depth1_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(txt_Depth1.Text, out double depth))
            {
                Plenum.Depth = depth;
            }
        }

        private void txt_FanCount1_TextChanged(object sender, EventArgs e)
        {
            if (int.TryParse(txt_FanCount1.Text, out int fanCount))
            {
                Plenum.FanCount = fanCount;
            }
        }

        private void checkBox_MidCol_CheckedChanged(object sender, EventArgs e)
        {
            Plenum.MidColumns = checkBox_MidCol.Checked;
        }


        // Column
        private bool isUserChange = true;

        private void textBox_Depth_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange && double.TryParse(textBox_Depth.Text, out double depth))
            {
                Beam.Depth = depth;
                PlenumColumn.Size = comboBox_ColumnSize.Text = "Custom";
            }
        }

        private void textBox_WebTHK_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange && double.TryParse(textBox_WebTHK.Text, out double webTHK))
            {
                Beam.WebTHK = webTHK;
                PlenumColumn.Size = comboBox_ColumnSize.Text = "Custom";
            }
        }

        private void textBox_FlangeWidth_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange && double.TryParse(textBox_FlangeWidth.Text, out double flangeWidth))
            {
                Beam.FlangeWidth = flangeWidth;
                PlenumColumn.Size = comboBox_ColumnSize.Text = "Custom";
            }
        }

        private void textBox_FlangeTHK_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange && double.TryParse(textBox_FlangeTHK.Text, out double flangeTHK))
            {
                Beam.FlangeTHK = flangeTHK;
                PlenumColumn.Size = comboBox_ColumnSize.Text = "Custom";
            }
        }

        private void textBox_K_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange && double.TryParse(textBox_K.Text, out double k))
            {
                Beam.K = k;
                PlenumColumn.Size = comboBox_ColumnSize.Text = "Custom";
            }
        }

        private void textBox_K1_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange && double.TryParse(textBox_K1.Text, out double k1))
            {
                Beam.K1 = k1;
                PlenumColumn.Size = comboBox_ColumnSize.Text = "Custom";
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
            txt_Length1.Text = Plenum.Length.ToString();
            txt_Width1.Text = Plenum.Width.ToString();
            txt_Depth1.Text = Plenum.Depth.ToString();
            checkBox_MidCol.Checked = Plenum.MidColumns;
            EndcomboBox1.Text = EndPanel.THK.ToString();
            SidecomboBox2.Text = SidePanel.THK.ToString();

            // Beam
            comboBox_ColumnSize.Text = PlenumColumn.Size;
            textBox_Depth.Text = Beam.Depth.ToString();
            textBox_WebTHK.Text = Beam.WebTHK.ToString();
            textBox_FlangeWidth.Text = Beam.FlangeWidth.ToString();
            textBox_FlangeTHK.Text = Beam.FlangeTHK.ToString();
            textBox_K.Text = Beam.K.ToString();
            textBox_K1.Text = Beam.K1.ToString();

            // Fan
            txt_FanCount1.Text = Plenum.FanCount.ToString();
            txt_FanDiameter.Text = Plenum._fanDiameterFeet.ToString();
            txt_RingDepth.Text = FanRing.Depth.ToString();

            checkBox1_MTRBeam.Checked = MotorBeamWld.Enabled;
            textBox1_columnLength.Text = PlenumColumn.Height.ToString();

            if (Plenum.MotorShaft.ToLower() == "down")
                comboBox1_driveDesign.Text = "Motor Shaft Down";
            else
                comboBox1_driveDesign.Text = "Motor Shaft Up";

            textBox_ExtraLength.Text = Johnson._extraLength == 0 ? "" : Johnson._extraLength.ToString();

            switch (Part.Material)
            {
                case MaterialSpec.A36:
                    materialCombo.Text = "A36";
                    break;
                case MaterialSpec.A572_50:
                    materialCombo.Text = "A572_50";
                    break;
            }

            txt_xShift.Text = FloorStiffener.XShiftAdjustment.ToString();
            lengthAdj.Text = FloorStiffener.LengthAdjustment.ToString();
            zShift_txt.Text = FloorStiffener.ZShiftAdjustment.ToString();

            checkBox1_locations.Checked = ToggleRelocate;
            checkBox2_dwg.Checked = ToggleCreateDrawing;
            checkBox3_save.Checked = ToggleSave;
            checkBox4_delete.Checked = ToggleDeleteFiles;
        }

        private void btn_Standard_Click(object sender, EventArgs e)
        {
            new Standard();
        }

        private void comboBox_ColumnSize_SelectedIndexChanged(object sender, EventArgs e)
        {
            isUserChange = false;

            PlenumColumn.Size = comboBox_ColumnSize.Text;
            if (PlenumColumn.Size != "Custom")
            {
                Beam.ResetSize();
                textBox_Depth.Text = Beam.Depth.ToString();
                textBox_WebTHK.Text = Beam.WebTHK.ToString();
                textBox_FlangeWidth.Text = Beam.FlangeWidth.ToString();
                textBox_FlangeTHK.Text = Beam.FlangeTHK.ToString();
                textBox_K.Text = Beam.K.ToString();
                textBox_K1.Text = Beam.K1.ToString();
            }

            isUserChange = true;
        }
        private void comboBox1_driveDesign_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (comboBox1_driveDesign.Text == "Motor Shaft Down")
                Plenum.MotorShaft = "Down";
            else
                Plenum.MotorShaft = "Up";
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
            if (double.TryParse(txt_FanDiameter.Text, out double fanDiameter))
            {
                Plenum.FanDiameter = fanDiameter;
            }
        }

        private void txt_RingDepth_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(txt_RingDepth.Text, out double ringDepth))
                FanRing.Depth = ringDepth;
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
                    EndPanel.THK = 0.1344;
                    break;
                case "0.1875":
                    EndPanel.THK = 0.1875;
                    break;
                case "0.2500":
                    EndPanel.THK = 0.2500;
                    break;
                case "0.3125":
                    EndPanel.THK = 0.3125;
                    break;
                case "0.3750":
                    EndPanel.THK = 0.3750;
                    break;
            }
        }

        private void sideComboBox2_SelectedIndexChanged(object sender, EventArgs e)
        {
            switch (SidecomboBox2.Text)
            {
                case "0.1344":
                    SidePanel.THK = 0.1344;
                    break;
                case "0.1875":
                    SidePanel.THK = 0.1875;
                    break;
                case "0.2500":
                    SidePanel.THK = 0.2500;
                    break;
                case "0.3125":
                    SidePanel.THK = 0.3125;
                    break;
                case "0.3750":
                    SidePanel.THK = 0.3750;
                    break;
            }
        }

        private void materialCombo_SelectedIndexChanged(object sender, EventArgs e)
        {
            switch (materialCombo.Text)
            {
                case "A36":
                    Part.Material = MaterialSpec.A36;
                    break;
                case "A572_50":
                    Part.Material = MaterialSpec.A572_50;
                    break;
            }
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
            new Standard(CallerType.Standard);
        }

        private void jhn_floor_Click(object sender, EventArgs e)
        {
            new Johnson(CallerType.Johnson);
        }

        private void leg_floor_Click(object sender, EventArgs e)
        {
            new Legacy(CallerType.Legacy);
        }

        private void checkBox1_locations_CheckedChanged(object sender, EventArgs e)
        {
            ToggleRelocate = checkBox1_locations.Checked;
        }

        private void checkBox2_dwg_CheckedChanged(object sender, EventArgs e)
        {
            ToggleCreateDrawing = checkBox2_dwg.Checked;
        }

        private void checkBox3_save_CheckedChanged(object sender, EventArgs e)
        {
            ToggleSave = checkBox3_save.Checked;
        }

        private void checkBox4_delete_CheckedChanged(object sender, EventArgs e)
        {
            ToggleDeleteFiles = checkBox4_delete.Checked;
        }

        private void button1_Click(object sender, EventArgs e)
        {
            mTools.Unlock();
        }
    }
}
