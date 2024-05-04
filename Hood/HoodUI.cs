using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Hood
{
    public partial class HoodUI : Form
    {
        public HoodUI()
        {
            InitializeComponent();
        }


        // Job
        private void txt_JobNumber_TextChanged(object sender, EventArgs e)
        {
            HoodData.Project = txt_JobNumber.Text;
        }
        private void txt_JobCustomer_TextChanged(object sender, EventArgs e)
        {
            HoodData.Customer = txt_JobCustomer.Text;
        }
        private void txt_JobClient_TextChanged(object sender, EventArgs e)
        {
            HoodData.Client = txt_JobClient.Text;
        }
        private void txt_JobLocation_TextChanged(object sender, EventArgs e)
        {
            HoodData.Location = txt_JobLocation.Text;
        }
        private void txt_JobPO_TextChanged(object sender, EventArgs e)
        {
            HoodData.PurchaseOrder = txt_JobPO.Text;
        }
        private void txt_JobItemNo_TextChanged(object sender, EventArgs e)
        {
            HoodData.ItemNumber = txt_JobItemNo.Text;
        }
        private void txt_Initials_TextChanged(object sender, EventArgs e)
        {
            HoodData.Initials = txt_Initials.Text;
        }


        // Hood
        private void txt_Bank_TextChanged(object sender, EventArgs e)
        {
            HoodData.Bank = txt_Bank.Text[0];
        }
        private void txt_PlenumCenterWidth_TextChanged(object sender, EventArgs e)
        {
            HoodData.Length = double.Parse(txt_Length.Text);
        }
        private void txt_Width_TextChanged(object sender, EventArgs e)
        {
            HoodData.Width = double.Parse(txt_Width.Text);
        }
        private void txt_Height_TextChanged(object sender, EventArgs e)
        {
            HoodData.Height = double.Parse(txt_Height.Text);
        }
        private void txt_FanDiameter_TextChanged(object sender, EventArgs e)
        {
            HoodData.FanDiameter = double.Parse(txt_FanDiameter.Text);
        }
        private void combo_Depth_SelectedIndexChanged(object sender, EventArgs e)
        {
            HoodData.Ring.Depth = int.Parse(combo_Depth.Text);
        }
        private void txt_Stacks_TextChanged(object sender, EventArgs e)
        {
            int stacks = int.Parse(txt_Stacks.Text);
            if (stacks < 1)
            {
                stacks = 1;
            }
            HoodData.Stacks = stacks;
        }
        private void combo_WindLoad_SelectedIndexChanged(object sender, EventArgs e)
        {
            HoodData.WindLoad = int.Parse(combo_WindLoad.Text);
        }


        // Refresh UI
        private void HoodUI_Load(object sender, EventArgs e)
        {
            // Job
            txt_JobNumber.Text = HoodData.Project;
            txt_JobCustomer.Text = HoodData.Customer;
            txt_JobClient.Text = HoodData.Client;
            txt_JobLocation.Text = HoodData.Location;
            txt_JobPO.Text = HoodData.PurchaseOrder;
            txt_JobItemNo.Text = HoodData.ItemNumber;
            txt_Initials.Text = HoodData.Initials;

            // Hood
            txt_Bank.Text = HoodData.Bank.ToString();
            txt_Length.Text = HoodData.Length.ToString();
            txt_Width.Text = HoodData.Width.ToString();
            txt_Height.Text = HoodData.Height.ToString();
            txt_FanDiameter.Text = HoodData.fanDiameterInFeet.ToString();
            combo_Depth.Text = HoodData.Ring.Depth.ToString();
            txt_Stacks.Text = HoodData.Stacks.ToString();
            combo_WindLoad.Text = HoodData.WindLoad.ToString();

            // Adv
            txt_Shift.Text = HoodData.Shift.ToString();
            txt_Adjust.Text = HoodData.Adjust.ToString();
        }
        private void btn_Create_Click(object sender, EventArgs e)
        {
            txt_JobNumber_TextChanged(sender, e);
            txt_JobCustomer_TextChanged(sender, e);
            txt_JobClient_TextChanged(sender, e);
            txt_JobLocation_TextChanged(sender, e);
            txt_JobPO_TextChanged(sender, e);
            txt_JobItemNo_TextChanged(sender, e);
            txt_Initials_TextChanged(sender, e);

            txt_Bank_TextChanged(sender, e);
            txt_PlenumCenterWidth_TextChanged(sender, e);
            txt_Width_TextChanged(sender, e);
            txt_Height_TextChanged(sender, e);
            txt_FanDiameter_TextChanged(sender, e);
            combo_Depth_SelectedIndexChanged(sender, e);
            txt_Stacks_TextChanged(sender, e);
            combo_WindLoad_SelectedIndexChanged(sender, e);

            txt_shift_TextChanged(sender, e);
            txt_Adjust_TextChanged(sender, e);

            new Hood();
        }
        private void btn_Edit_Click(object sender, EventArgs e)
        {
            var selectedComponents = HoodData.SelectedComponents();
            new Hood(selectedComponents);
        }


        // Advanced Options
        private void button11_Click(object sender, EventArgs e)
        {
            Tools.ModelTools.EnablePartUI();
        }
        private void txt_shift_TextChanged(object sender, EventArgs e)
        {
            HoodData.Shift = double.TryParse(txt_Shift.Text, out double value) ? value : HoodData.Adjust;
        }
        private void txt_Adjust_TextChanged(object sender, EventArgs e)
        {
            HoodData.Adjust = double.TryParse(txt_Adjust.Text, out double value) ? value : HoodData.Adjust;
        }
        private void button1_Click(object sender, EventArgs e)
        {
            btn_Edit_Click(sender, e);
        }
    }
}
