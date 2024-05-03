using FileTools;
<<<<<<< HEAD
using ModelTools;
using Structure.Columns.Derived.Children;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using static System.Windows.Forms.VisualStyles.VisualStyleElement;
=======
using Structure.Braces;
using Structure.Columns.Derived.Children;
using System;
using System.Windows.Forms;
using static Tools.ModelTools;
using static FileTools.SharedProperties;
>>>>>>> releases/v4.0.0

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

<<<<<<< HEAD
=======
        private DataGridView dataGridView;
>>>>>>> releases/v4.0.0
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
<<<<<<< HEAD
=======
        private void button1_Click(object sender, EventArgs e)
        {
            new Structure(25, "Structure");
        }
        private void button1_save_Click(object sender, EventArgs e)
        {
            SaveEverything();
        }
>>>>>>> releases/v4.0.0

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
<<<<<<< HEAD
            height_TextBox.Text = SharedProperties.ColumnHeight.ToString();
            midColumns_Box.Checked = SharedProperties.MidColumns;
            rotate_Box.Checked = Beam.Rotate;
=======
            height_TextBox.Text = SharedProperties.TotalColumnHeight.ToString();
            midColumns_Box.Checked = SharedProperties.MidColumns;
            rotate_Box.Checked = Beam.IsRotated;
            beamSize_Box.Text = Beam.Size;
            textBox_Depth.Text = Beam.Depth.ToString();
            textBox_WebTHK.Text = Beam.WebTHK.ToString();
            textBox_FlangeWidth.Text = Beam.FlangeWidth.ToString();
            textBox_FlangeTHK.Text = Beam.FlangeTHK.ToString();
            textBox_K.Text = Beam.K.ToString();
            textBox_K1.Text = Beam.K1.ToString();
>>>>>>> releases/v4.0.0

            #endregion
            #region BasePlate_Load

<<<<<<< HEAD
            bpWidth_Box.Text = BasePlate.Width.ToString();
            bpLength_Box.Text = BasePlate.Length.ToString();
=======
            bpWidth_Box.Text = BasePlate.LocalWidth.ToString();
            bpLength_Box.Text = BasePlate.LocalLength.ToString();
>>>>>>> releases/v4.0.0
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
<<<<<<< HEAD

=======
            #region Brace_Load

            // General
            braceType_Box.Text = SharedProperties.BraceType;
            clipTHK_Box.Text = Clip.THK.ToString();
            braceHoleDiameter_Box.Text = Clip.HoleDiameter.ToString();
            braceAngle_Box.Text = SharedProperties.BraceAngle.ToString();

            #endregion
            #region Toggles_Load

            createDrawing_Toggle.Checked = ToggleCreateDrawing;
            save_Toggle.Checked = ToggleSave;
            delete_Toggle.Checked = ToggleDeleteFiles;

            #endregion
>>>>>>> releases/v4.0.0
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
<<<<<<< HEAD
                SharedProperties.ColumnHeight = value;
=======
                SharedProperties.TotalColumnHeight = value;
>>>>>>> releases/v4.0.0
        }

        private void midColumn_Box_CheckedChanged(object sender, EventArgs e)
        {
            SharedProperties.MidColumns = midColumns_Box.Checked;
        }
        private void rotate_Box_CheckedChanged(object sender, EventArgs e)
        {
<<<<<<< HEAD
            Beam.Rotate = rotate_Box.Checked;
        }

=======
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
>>>>>>> releases/v4.0.0

        #endregion
        #region BasePlate_Changed

        private void bpWidth_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(bpWidth_Box.Text, out double value))
<<<<<<< HEAD
                BasePlate.Width = value;
=======
                BasePlate.LocalWidth = value;
>>>>>>> releases/v4.0.0
        }

        private void bpLength_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(bpLength_Box.Text, out double value))
<<<<<<< HEAD
                BasePlate.Length = value;
=======
                BasePlate.LocalLength = value;
>>>>>>> releases/v4.0.0
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
<<<<<<< HEAD

=======
>>>>>>> releases/v4.0.0
        #region MachineryMount_Changed

        private void mmHeight_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(mmHeight_Box.Text, out double value))
                SharedProperties.MachineryMountHeight = value;
        }

<<<<<<< HEAD
=======

        #endregion
        #region Braced_Changed

        // General
        private void braceType_Box_SelectedIndexChanged_1(object sender, EventArgs e)
        {
            SharedProperties.BraceType = braceType_Box.Text;
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
                depthWT_Box.ReadOnly = false; depthWT_Box.Text = BraceT.Depth.ToString();
                stemTHKWT_Box.ReadOnly = false; stemTHKWT_Box.Text = BraceT.StemTHK.ToString();
                flangeWidthWT_Box.ReadOnly = false; flangeWidthWT_Box.Text = BraceT.FlangeWidth.ToString();
                flangeTHKWT_Box.ReadOnly = false; flangeTHKWT_Box.Text = BraceT.FlangeTHK.ToString();
                kWT_Box.ReadOnly = false; kWT_Box.Text = BraceT.K.ToString();
                k1WT_Box.ReadOnly = false; k1WT_Box.Text = BraceT.K1.ToString();
                flangeGageWT_Box.ReadOnly = false; flangeGageWT_Box.Text = BraceT.FlangeGage.ToString();
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
                Clip.THK = value;
        }
        private void braceHoleDiameter_Box_TextChanged_1(object sender, EventArgs e)
        {
            if (double.TryParse(braceHoleDiameter_Box.Text, out double value))
                Clip.HoleDiameter = value;
        }
        private void braceAngle_Box_TextChanged_1(object sender, EventArgs e)
        {
            if (double.TryParse(braceAngle_Box.Text, out double value))
                SharedProperties.BraceAngle = value;
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
                BraceT.Depth = value;
        }
        private void stemTHKWT_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(stemTHKWT_Box.Text, out double value))
                BraceT.StemTHK = value;
        }
        private void flangeWidthWT_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(flangeWidthWT_Box.Text, out double value))
                BraceT.FlangeWidth = value;
        }
        private void flangeTHKWT_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(flangeTHKWT_Box.Text, out double value))
                BraceT.FlangeTHK = value;
        }
        private void kWT_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(kWT_Box.Text, out double value))
                BraceT.K = value;
        }
        private void k1WT_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(k1WT_Box.Text, out double value))
                BraceT.K1 = value;
        }
        private void flangeGageWT_Box_TextChanged(object sender, EventArgs e)
        {
            if (double.TryParse(flangeGageWT_Box.Text, out double value))
                BraceT.FlangeGage = value;
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

>>>>>>> releases/v4.0.0
        #endregion
    }
}
