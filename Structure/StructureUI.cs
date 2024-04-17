using FileTools;
using Structure.Columns.Derived.Children;
using System;
using System.Windows.Forms;

namespace Structure
{
    public partial class StructureUI : Form
    {
        public StructureUI()
        {
            InitializeComponent();

            SharedProperties.OnFanCountChanged += SharedProperties_OnFanCountChanged;
        }
        #region CustomEvents

        private void SharedProperties_OnFanCountChanged()
        {
            this.Invoke((MethodInvoker)delegate
            {
                midColumns_Box.Checked = SharedProperties.MidColumns;
            });
        }

        #endregion
        #region Buttons

        private void btn_Standard_Click(object sender, EventArgs e)
        {
            new Structure(25, "Structure");
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

            width_TextBox.Text = SharedProperties.Width.ToString();
            length_TextBox.Text = SharedProperties.Length.ToString();
            height_TextBox.Text = SharedProperties.ColumnHeight.ToString();
            midColumns_Box.Checked = SharedProperties.MidColumns;
            rotate_Box.Checked = Beam.IsRotated;
            beamSize_Box.Text = Beam.Size;
            textBox_Depth.Text = Beam.Depth.ToString();
            textBox_WebTHK.Text = Beam.WebTHK.ToString();
            textBox_FlangeWidth.Text = Beam.FlangeWidth.ToString();
            textBox_FlangeTHK.Text = Beam.FlangeTHK.ToString();
            textBox_K.Text = Beam.K.ToString();
            textBox_K1.Text = Beam.K1.ToString();

            #endregion
            #region BasePlate_Load

            bpWidth_Box.Text = BasePlate.LocalWidth.ToString();
            bpLength_Box.Text = BasePlate.LocalLength.ToString();
            wSPA_Box.Text = BasePlate.WidthHoleSpacing.ToString();
            lSPA_Box.Text = BasePlate.LengthHoleSpacing.ToString();
            dia_Box.Text = BasePlate.HoleDiameter.ToString();

            #endregion
            #region Plenum_Load

            fanCount_Box.Text = SharedProperties.FanCount.ToString();
            depth_Box.Text = SharedProperties.PlenumDepth.ToString();

            #endregion
            #region MachineryMount_Load

            mmHeight_Box.Text = SharedProperties.MachineryMountHeight.ToString();

            #endregion
            #region Brace_Load

            braceType_Box.Text = SharedProperties.BraceType;
            clipTHK_Box.Text = Clip.THK.ToString();
            braceHoleDiameter_Box.Text = Clip.HoleDiameter.ToString();
            braceAngle_Box.Text = SharedProperties.BraceAngle.ToString();

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
                SharedProperties.Width = value;
        }

        private void length_TextBox_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(length_TextBox.Text, out double value))
                SharedProperties.Length = value;
        }

        private void height_TextBox_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(height_TextBox.Text, out double value))
                SharedProperties.ColumnHeight = value;
        }

        private void midColumn_Box_CheckedChanged(object sender, EventArgs e)
        {
            SharedProperties.MidColumns = midColumns_Box.Checked;
        }
        private void rotate_Box_CheckedChanged(object sender, EventArgs e)
        {
            Beam.IsRotated = rotate_Box.Checked;
        }
        private bool isUserChange = true;
        private void beamSize_Box_SelectedIndexChanged(object sender, EventArgs e)
        {
            isUserChange = false;

            Beam.Size = beamSize_Box.Text;
            if (Beam.Size != "Custom")
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
        private void textBox_Depth_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange && double.TryParse(textBox_Depth.Text, out double depth))
            {
                Beam.Depth = depth;
                Beam.Size = beamSize_Box.Text = "Custom";
            }
        }

        private void textBox_WebTHK_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange && double.TryParse(textBox_WebTHK.Text, out double webTHK))
            {
                Beam.WebTHK = webTHK;
                Beam.Size = beamSize_Box.Text = "Custom";
            }
        }

        private void textBox_FlangeWidth_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange && double.TryParse(textBox_FlangeWidth.Text, out double flangeWidth))
            {
                Beam.FlangeWidth = flangeWidth;
                Beam.Size = beamSize_Box.Text = "Custom";
            }
        }

        private void textBox_FlangeTHK_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange && double.TryParse(textBox_FlangeTHK.Text, out double flangeTHK))
            {
                Beam.FlangeTHK = flangeTHK;
                Beam.Size = beamSize_Box.Text = "Custom";
            }
        }

        private void textBox_K_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange && double.TryParse(textBox_K.Text, out double k))
            {
                Beam.K = k;
                Beam.Size = beamSize_Box.Text = "Custom";
            }
        }

        private void textBox_K1_TextChanged(object sender, EventArgs e)
        {
            if (isUserChange && double.TryParse(textBox_K1.Text, out double k1))
            {
                Beam.K1 = k1;
                Beam.Size = beamSize_Box.Text = "Custom";
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
                SharedProperties.FanCount = value;
        }
        private void depth_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(depth_Box.Text, out double value))
                SharedProperties.PlenumDepth = value;
        }


        #endregion
        #region MachineryMount_Changed

        private void mmHeight_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(mmHeight_Box.Text, out double value))
                SharedProperties.MachineryMountHeight = value;
        }


        #endregion
        #region Braced_Changed

        private void braceType_Box_SelectedIndexChanged(object sender, EventArgs e)
        {
            SharedProperties.BraceType = braceType_Box.Text;
        }
        private void thk_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(clipTHK_Box.Text, out double value))
                Clip.THK = value;
        }
        private void braceHoleDiameter_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(braceHoleDiameter_Box.Text, out double value))
                Clip.HoleDiameter = value;
        }
        private void braceAngle_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(braceAngle_Box.Text, out double value))
                SharedProperties.BraceAngle = value;
        }

        #endregion


    }
}
