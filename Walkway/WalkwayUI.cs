using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using Walkway.Tools;
using static System.Windows.Forms.VisualStyles.VisualStyleElement.Rebar;

namespace Walkway
{
    public partial class WalkwayUI : Form
    {
        public WalkwayUI()
        {
            InitializeComponent();
        }

        private void btn_WalkwayCreate_Click(object sender, EventArgs e)
        {
            DevTools.DisablePartUI();

            Walkway.Create_Standard_EndWalkway
                (
                Walkway.Bank, 
                Walkway.Width, 
                Walkway.RailHeight, 
                Walkway.FloorHeight, 
                Walkway.MinStringerSize, 
                Walkway.OffsetFromColumnCenter, 
                Walkway.AcheColumnSize, 
                Walkway.AcheColumnCenterToCenterWidth, 
                Walkway.EndToSupportCenter
                );
            
            DevTools.EnablePartUI();
            DevTools.Rebuild();
        }
        private void btn_WalkwayEdit_Click(object sender, EventArgs e)
        {
            DevTools.DisablePartUI();

            Walkway.Modify_Walkway
                (
                Walkway.SelectedComponents(),
                Walkway.Width,
                Walkway.RailHeight,
                Walkway.FloorHeight,
                Walkway.MinStringerSize,
                Walkway.OffsetFromColumnCenter,
                Walkway.AcheColumnSize,
                Walkway.AcheColumnCenterToCenterWidth,
                Walkway.EndToSupportCenter,
                false);
            
            DevTools.EnablePartUI();
            DevTools.Rebuild();
        }

        private void CreatePlatform_Click(object sender, EventArgs e)
        {
            DevTools.DisablePartUI();

            new WalkwayPlatform
                (
                Walkway.Bank,
                Walkway.Length,
                Walkway.Width,
                Walkway.FloorHeight,
                Walkway.MinStringerSize,
                Walkway.EndToSupportCenter,
                true
                );

            DevTools.EnablePartUI();
            DevTools.Rebuild();
        }
        private void EditPlatform_Click(object sender, EventArgs e)
        {
            DevTools.DisablePartUI();

            new WalkwayPlatform
                (
                Walkway.SelectedComponents(),
                Walkway.Length,
                Walkway.Width,
                Walkway.FloorHeight,
                Walkway.MinStringerSize,
                Walkway.EndToSupportCenter,
                Walkway.UpdateLocation
                );

            DevTools.EnablePartUI();
            DevTools.Rebuild();
        }

        private void NewRail_Click(object sender, EventArgs e)
        {
            DevTools.DisablePartUI();

            new HandRail
                (
                Walkway.Bank,
                Walkway.Length,
                Walkway.RailHeight,
                Walkway.FloorHeight,
                Walkway.MinStringerSize,
                true
                );

            DevTools.EnablePartUI();
            DevTools.Rebuild();
        }
        private void ExistingRail_Click(object sender, EventArgs e)
        {
            DevTools.DisablePartUI();

            new HandRail
                (
                Walkway.SelectedComponents(),
                Walkway.Length,
                Walkway.RailHeight,
                Walkway.FloorHeight,
                Walkway.MinStringerSize,
                Walkway.UpdateLocation
                );

            DevTools.EnablePartUI();
            DevTools.Rebuild();
        }

        private void NewSupport_Click(object sender, EventArgs e)
        {
            DevTools.DisablePartUI();

            new Support
                (
                Walkway.Bank,
                Walkway.MinStringerSize,
                Walkway.Width,
                Walkway.EndToSupportCenter,
                Walkway.AcheColumnSize,
                Walkway.Length,
                true
                );

            DevTools.EnablePartUI();
            DevTools.Rebuild();
        }
        private void EditSupport_Click(object sender, EventArgs e)
        {
            DevTools.DisablePartUI();

            new Support
                (
                Walkway.SelectedComponents(),
                Walkway.MinStringerSize,
                Walkway.Width,
                Walkway.OffsetFromColumnCenter,
                Walkway.AcheColumnSize,
                Walkway.Length,
                Walkway.UpdateLocation
                );

            DevTools.EnablePartUI();
            DevTools.Rebuild();
        }


        //----------------------------------------


        // Job
        private void txt_JobNumber_TextChanged(object sender, EventArgs e)
        {
            Walkway.Project = txt_JobNumber.Text;
        }
        private void txt_JobCustomer_TextChanged(object sender, EventArgs e)
        {
            Walkway.Customer = txt_JobCustomer.Text;
        }
        private void txt_JobClient_TextChanged(object sender, EventArgs e)
        {
            Walkway.Client = txt_JobClient.Text;
        }
        private void txt_JobLocation_TextChanged(object sender, EventArgs e)
        {
            Walkway.Location = txt_JobLocation.Text;
        }
        private void txt_JobPO_TextChanged(object sender, EventArgs e)
        {
            Walkway.PurchaseOrder = txt_JobPO.Text;
        }
        private void txt_JobItemNo_TextChanged(object sender, EventArgs e)
        {
            Walkway.ItemNumber = txt_JobItemNo.Text;
        }
        private void txt_Initials_TextChanged(object sender, EventArgs e)
        {
            Walkway.Initials = txt_Initials.Text;
        }

        // Walkway
        private void txt_WalkwayBank_TextChanged(object sender, EventArgs e)
        {
            Walkway.Bank = txt_WalkwayBank.Text[0];
        }
        private void txt_WalkwayPlenumCenterWidth_TextChanged(object sender, EventArgs e)
        {
            Walkway.AcheColumnCenterToCenterWidth = double.Parse(txt_WalkwayPlenumCenterWidth.Text);
        }
        private void txt_WalkwayWidth_TextChanged(object sender, EventArgs e)
        {
            Walkway.Width = double.Parse(txt_WalkwayWidth.Text);
        }
        private void txt_WalkwayFloorHeight_TextChanged(object sender, EventArgs e)
        {
            Walkway.FloorHeight = double.Parse(txt_WalkwayFloorHeight.Text);
        }
        private void txt_WalkwayOffsetFromColumnCenter_TextChanged(object sender, EventArgs e)
        {
            Walkway.OffsetFromColumnCenter = double.Parse(txt_WalkwayOffsetFromColumnCenter.Text);
        }
        private void combo_WalkwayMinimumStringerSize_SelectedIndexChanged(object sender, EventArgs e)
        {
            var stringerSizeMapping = new Dictionary<string, int>
            {
                { "None", 0 },
                { "C6", 6 },
                { "C8", 8 },
                { "C10", 10 }
            };

            stringerSizeMapping.TryGetValue(combo_WalkwayMinimumStringerSize.Text, out int size);
            Walkway.MinStringerSize = size;
        }

        private void combo_WalkwayColumnSize_SelectedIndexChanged(object sender, EventArgs e)
        {
            Walkway.AcheColumnSize = combo_WalkwayColumnSize.Text;
        }
        private void txt_WalkwayRailHeight_TextChanged(object sender, EventArgs e)
        {
            Walkway.RailHeight = double.Parse(txt_WalkwayRailHeight.Text);
        }
        private void txt_WalkwaySupportCenterToWalkwayEnd_TextChanged(object sender, EventArgs e)
        {
            Walkway.EndToSupportCenter = double.Parse(txt_WalkwaySupportCenterToWalkwayEnd.Text);
        }

        // Platform
        private void txt_PlatformLength_TextChanged(object sender, EventArgs e)
        {
            Walkway.Length = double.Parse(txt_PlatformLength.Text);
        }
        private void txt_PlatformWidth_TextChanged(object sender, EventArgs e)
        {
            Walkway.Width = double.Parse(txt_PlatformWidth.Text);
        }
        private void txt_PlatformFloorHeight_TextChanged(object sender, EventArgs e)
        {
            Walkway.FloorHeight = double.Parse(txt_PlatformFloorHeight.Text);
        }
        private void txt_PlatformSupportCenterToWalkwayEnd_TextChanged(object sender, EventArgs e)
        {
            Walkway.EndToSupportCenter = double.Parse(txt_PlatformSupportCenterToWalkwayEnd.Text);
        }
        private void combo_PlatformMinimumStringerSize_SelectedIndexChanged(object sender, EventArgs e)
        {
            var stringerSizeMapping = new Dictionary<string, int>
            {
                { "None", 0 },
                { "C6", 6 },
                { "C8", 8 },
                { "C10", 10 }
            };

            stringerSizeMapping.TryGetValue(combo_PlatformMinimumStringerSize.Text, out int size);
            Walkway.MinStringerSize = size;
        }

        // Handrail
        private void txt_HandrailLength_TextChanged(object sender, EventArgs e)
        {
            Walkway.Length = double.Parse(txt_HandrailLength.Text);
        }
        private void txt_HandrailRailHeight_TextChanged(object sender, EventArgs e)
        {
            Walkway.RailHeight = double.Parse(txt_HandrailRailHeight.Text);
        }
        private void txt_HandrailFloorHeight_TextChanged(object sender, EventArgs e)
        {
            Walkway.FloorHeight = double.Parse(txt_HandrailFloorHeight.Text);
        }
        private void combo_HandrailMinimumStringerSize_SelectedIndexChanged(object sender, EventArgs e)
        {
            var stringerSizeMapping = new Dictionary<string, int>
            {
                { "None", 0 },
                { "C6", 6 },
                { "C8", 8 },
                { "C10", 10 }
            };

            stringerSizeMapping.TryGetValue(combo_HandrailMinimumStringerSize.Text, out int size);
            Walkway.MinStringerSize = size;
        }

        // Support
        private void txt_SupportWalkwayLength_TextChanged(object sender, EventArgs e)
        {
            Walkway.Length = double.Parse(txt_SupportWalkwayLength.Text);
        }
        private void txt_SupportWalkwayWidth_TextChanged(object sender, EventArgs e)
        {
            Walkway.Width = double.Parse(txt_SupportWalkwayWidth.Text);
        }
        private void txt_SupportOffsetFromColumnCenter_TextChanged(object sender, EventArgs e)
        {
            Walkway.OffsetFromColumnCenter = double.Parse(txt_SupportOffsetFromColumnCenter.Text);
        }
        private void combo_SupportColumnSize_SelectedIndexChanged(object sender, EventArgs e)
        {
            Walkway.AcheColumnSize = combo_SupportColumnSize.Text;
        }
        private void combo_SupportStringerSize_SelectedIndexChanged(object sender, EventArgs e)
        {
            var stringerSizeMapping = new Dictionary<string, int>
            {
                { "None", 0 },
                { "C6", 6 },
                { "C8", 8 },
                { "C10", 10 }
            };

            stringerSizeMapping.TryGetValue(combo_SupportStringerSize.Text, out int size);
            Walkway.MinStringerSize = size;
        }

        // Advanced Options
        private void UpdateLocation_CheckedChanged(object sender, EventArgs e)
        {
            CheckBox checkBox = (CheckBox)sender;

            if (checkBox.Checked)
            {
                Walkway.UpdateLocation = true;
            }
            else
            {
                Walkway.UpdateLocation = false;
            }
        }


        //-----------------------


        private void WalkwayUI_Load(object sender, EventArgs e)
        {
            var stringerSizeMapping = new Dictionary<int, string>
            {
                { 0, "None" },
                { 6, "C6" },
                { 8, "C8" },
                { 10, "C10" }
            };
            string minStringerSize = stringerSizeMapping.TryGetValue(Walkway.MinStringerSize, out var value) ? value : default(string);

            // Job
            txt_JobNumber.Text = Walkway.Project;
            txt_JobCustomer.Text = Walkway.Customer;
            txt_JobClient.Text = Walkway.Client;
            txt_JobLocation.Text = Walkway.Location;
            txt_JobPO.Text = Walkway.PurchaseOrder;
            txt_JobItemNo.Text = Walkway.ItemNumber;
            txt_Initials.Text = Walkway.Initials;

            // Walkway
            txt_WalkwayBank.Text = Walkway.Bank.ToString();
            txt_WalkwayPlenumCenterWidth.Text = Walkway.AcheColumnCenterToCenterWidth.ToString();
            txt_WalkwayWidth.Text = Walkway.Width.ToString();
            txt_WalkwayFloorHeight.Text = Walkway.FloorHeight.ToString();
            txt_WalkwayOffsetFromColumnCenter.Text = Walkway.OffsetFromColumnCenter.ToString();
            combo_WalkwayColumnSize.Text = Walkway.AcheColumnSize;
            txt_WalkwayRailHeight.Text = Walkway.RailHeight.ToString();
            txt_WalkwaySupportCenterToWalkwayEnd.Text = Walkway.EndToSupportCenter.ToString();
            combo_WalkwayMinimumStringerSize.Text = minStringerSize;

            // Platform
            txt_PlatformLength.Text = Walkway.Length.ToString();
            txt_PlatformWidth.Text = Walkway.Width.ToString();
            txt_PlatformFloorHeight.Text = Walkway.FloorHeight.ToString();
            txt_PlatformSupportCenterToWalkwayEnd.Text = Walkway.EndToSupportCenter.ToString();
            combo_PlatformMinimumStringerSize.Text = minStringerSize;

            // Handrail
            txt_HandrailLength.Text = Walkway.Length.ToString();
            txt_HandrailRailHeight.Text = Walkway.RailHeight.ToString();
            txt_HandrailFloorHeight.Text = Walkway.FloorHeight.ToString();
            combo_HandrailMinimumStringerSize.Text = minStringerSize;

            // Supports
            txt_SupportWalkwayLength.Text = Walkway.Length.ToString();
            txt_SupportWalkwayWidth.Text = Walkway.Width.ToString();
            txt_SupportOffsetFromColumnCenter.Text = Walkway.OffsetFromColumnCenter.ToString();
            combo_SupportColumnSize.Text = Walkway.AcheColumnSize;
            combo_SupportStringerSize.Text = minStringerSize;

            // Advanced Options
            UpdateLocations.Checked = Walkway.UpdateLocation;
        }

        private void EndRail_Click(object sender, EventArgs e)
        {
            Walkway.AddNewComponent("1351");
        }

        private void EndToePlate_Click(object sender, EventArgs e)
        {
            Walkway.AddNewComponent("1354");
        }

        private void MidPost_Click(object sender, EventArgs e)
        {
            Walkway.AddNewSubComponent("1344");
        }

        private void Strut_Click(object sender, EventArgs e)
        {
            Walkway.AddNewSubComponent("1339");
        }

        private void PartUI_Click(object sender, EventArgs e)
        {
            DevTools.EnablePartUI();
        }
    }
}
