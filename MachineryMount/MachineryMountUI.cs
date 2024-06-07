using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using static FileTools.CommonData.CommonData;
using static FileTools.Properties.Settings;

namespace MachineryMount
{
    public partial class MachineryMountUI : Form
    {
        #region Constructor, Buttons, Events

        public MachineryMountUI()
        {
            InitializeComponent();
        }
        private void bCreateUpdate_Click(object sender, EventArgs e)
        {
            new MachineryMount(4, nameof(MachineryMount));
        }
        private void MachineryMountUI_FormClosing(object sender, FormClosingEventArgs e)
        {
            SaveSettings();
        }

        #endregion
        private void MachineryMountUI_Load(object sender, EventArgs e)
        {
            // Main inputs
            cMMWidth.Text = MachineryMount_Width.ToString();
            dFrame.Text = MotorFrameSize.ToString() + "T";
            cFanShaftDiameter.Text = FanShaft_Diameter.ToString();
            tCenterToCenter.Text = MotorCenter_To_FanCenter.ToString();
            cVibrationSensor.Text = Vibration_Sensor;

            // Design selection
            cForced.Checked = ForcedDraft;
            cInduced.Checked = Induced;
            cBelt.Checked = BeltDrive;
            cGear.Checked = GearDrive;
            cDirect.Checked = DirectDrive;
            cDown.Checked = MotorShaftDown;
            cUp.Checked = MotorShaftUp;

            // Job info
            textBox_Bank.Text = Bank.ToString();
            job_Box.Text = Project;
            customer_Box.Text = Customer;
            client_Box.Text = Client;
            location_Box.Text = PlantLocation;
            purchaseOrder_Box.Text = PurchaseOrder;
            itemNumber_Box.Text = ItemNumber;
            initials_Box.Text = Initials;

            // External
            materialCombo.Text = MaterialSpecSetting;
            plenumWidth_TextBox.Text = Plenum_Width.ToString();
            tWeight.Text = TotalUnitWeight.ToString();
            tFanRingDepth.Text = FanRing_Depth.ToString();
            cPlenumStyle.Text = Plenum_Design.ToString();

            // Advanced
            cStringer.Text = Stringer_Size;
            tHeight.Text = MachineryMount_Height.ToString();
            createDrawing_Toggle.Checked = Toggle_CreateDrawing;
            save_Toggle.Checked = Toggle_Save;
            delete_Toggle.Checked = Toggle_DeleteFiles;
            lock_StringerSize.Checked = Lock_StringerSize;
            lock_MMHeight.Checked = Lock_MachineryMountHeight;
        }

        #region Main inputs

        private void cMMWidth_Leave(object sender, EventArgs e)
        {
            UI_DoubleChanged(cMMWidth.Text, x => MachineryMount_Width = x);
            cMMWidth.Text = MachineryMount_Width.ToString();
        }
        private void dFrame_SelectedIndexChanged(object sender, EventArgs e)
        {
            UI_IntChanged(dFrame.Text.Substring(0, dFrame.Text.Length - 1), x => MotorFrameSize = x);
            dFrame.Text = MotorFrameSize.ToString() + "T";

            // Update dependents
            tHeight.Text = MachineryMount_Height.ToString();
        }
        private void cFanShaftDiameter_SelectedIndexChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(cFanShaftDiameter.Text, x => FanShaft_Diameter = x);
        }
        private void tCenterToCenter_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tCenterToCenter.Text, x => MotorCenter_To_FanCenter = x);
        }
        private void cVibrationSensor_SelectedIndexChanged(object sender, EventArgs e)
        {
            UI_StringChanged(cVibrationSensor.Text, x => Vibration_Sensor = x);
        }

        #endregion
        #region Design selection

        private void cForced_CheckedChanged(object sender, EventArgs e)
        {
            try
            {
                UI_BoolChanged(cForced.Checked, x => ForcedDraft = x);
                cInduced.Checked = Induced;
            }
            catch (InvalidOperationException ex)
            {
                Debug.WriteLine(ex.Message);
                cForced.Checked = true;
            }
        }
        private void cInduced_CheckedChanged(object sender, EventArgs e)
        {
            try
            {
                UI_BoolChanged(cInduced.Checked, x => Induced = x);
                cForced.Checked = ForcedDraft;
            }
            catch (InvalidOperationException ex)
            {
                Debug.WriteLine(ex.Message);
                cInduced.Checked = true;
            }

        }
        private void cBelt_CheckedChanged(object sender, EventArgs e)
        {
            try
            {
                UI_BoolChanged(cBelt.Checked, x => BeltDrive = x);
                cGear.Checked = GearDrive;
                cDirect.Checked = DirectDrive;
            }
            catch (InvalidOperationException ex)
            {
                Debug.WriteLine(ex.Message);
                cBelt.Checked = true;
            }

        }
        private void cGear_CheckedChanged(object sender, EventArgs e)
        {
            try
            {
                UI_BoolChanged(cGear.Checked, x => GearDrive = x);
                cBelt.Checked = BeltDrive;
                cDirect.Checked = DirectDrive;
            }
            catch (InvalidOperationException ex)
            {
                Debug.WriteLine(ex.Message);
                cGear.Checked = true;
            }

        }
        private void cDirect_CheckedChanged(object sender, EventArgs e)
        {
            try
            {
                UI_BoolChanged(cDirect.Checked, x => DirectDrive = x);
                cBelt.Checked = BeltDrive;
                cGear.Checked = GearDrive;
            }
            catch (InvalidOperationException ex)
            {
                Debug.WriteLine(ex.Message);
                cDirect.Checked = true;
            }

        }
        private void cDown_CheckedChanged(object sender, EventArgs e)
        {
            try
            {
                UI_BoolChanged(cDown.Checked, x => MotorShaftDown = x);
                cUp.Checked = MotorShaftUp;
            }
            catch (InvalidOperationException ex)
            {
                Debug.WriteLine(ex.Message);
                cDown.Checked = true;
            }

        }
        private void cUp_CheckedChanged(object sender, EventArgs e)
        {
            try
            {
                UI_BoolChanged(cUp.Checked, x => MotorShaftUp = x);
                cDown.Checked = MotorShaftDown;
            }
            catch (InvalidOperationException ex)
            {
                Debug.WriteLine(ex.Message);
                cUp.Checked = true;
            }

        }

        #endregion
        #region External

        private void plenumWidth_TextBox_Leave(object sender, EventArgs e)
        {
            UI_DoubleChanged(plenumWidth_TextBox.Text, x => Plenum_Width = x);

            // Update dependents
            cStringer.Text = Stringer_Size;
        }
        private void materialCombo_SelectedIndexChanged(object sender, EventArgs e)
        {
            UI_StringChanged(materialCombo.Text, x => MaterialSpecSetting = x);
        }
        private void tWeight_Leave(object sender, EventArgs e)
        {
            UI_IntChanged(tWeight.Text, x => TotalUnitWeight = x);

            // Update dependents
            cStringer.Text = Stringer_Size;
        }
        private void tFanRingDepth_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tFanRingDepth.Text, x => FanRing_Depth = x);
        }
        private void cPlenumStyle_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (Enum.TryParse(cPlenumStyle.Text, out Design design))
            {
                Plenum_Design = design;
            }
            else throw new NotImplementedException();
            SaveSettings();
        }

        #endregion
        #region Job info

        private void textBox_Bank_TextChanged(object sender, EventArgs e)
        {
            UI_CharChanged(textBox_Bank.Text, x => Bank = x);

            // Update dependents
            Lock_StringerSize = false;
            lock_StringerSize.Checked = Lock_StringerSize;
            Lock_MachineryMountHeight = false;
            lock_MMHeight.Checked = Lock_MachineryMountHeight;
        }
        private void job_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(job_Box.Text, x => Project = x);

            // Update dependents
            Lock_StringerSize = false;
            lock_StringerSize.Checked = Lock_StringerSize;
            Lock_MachineryMountHeight = false;
            lock_MMHeight.Checked = Lock_MachineryMountHeight;
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

        private void cStringer_Leave(object sender, EventArgs e)
        {
            try
            {
                UI_StringChanged(cStringer.Text, x => Stringer_Size = x);
                cStringer.Text = Stringer_Size;
            }
            catch (InvalidOperationException ex)
            {
                MessageBox.Show(ex.Message, nameof(InvalidOperationException), MessageBoxButtons.OK, MessageBoxIcon.Error);
                cStringer.Text = Stringer_Size;
            }
        }
        private void tHeight_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tHeight.Text, x => MachineryMount_Height = x);
        }
        private void createDrawing_Toggle_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(createDrawing_Toggle.Checked, x => Toggle_CreateDrawing = x);
        }
        private void save_Toggle_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(save_Toggle.Checked, x => Toggle_Save = x);
        }
        private void delete_Toggle_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(delete_Toggle.Checked, x => Toggle_DeleteFiles = x);
        }
        private void lock_StringerSize_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(lock_StringerSize.Checked, x => Lock_StringerSize = x);
            cStringer.Enabled = !Lock_StringerSize;
        }
        private void lock_MMHeight_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(lock_MMHeight.Checked, x => Lock_MachineryMountHeight = x);
            tHeight.Enabled = !Lock_MachineryMountHeight;
        }








        #endregion
    }
}
