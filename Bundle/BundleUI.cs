using System;
using System.Windows.Forms;
using static FileTools.CommonData.CommonData;
using static FileTools.Properties.Settings;

namespace Bundle
{
    public partial class BundleUI : Form
    {
        public BundleUI()
        {
            InitializeComponent();
        }
        private void BundleUI_Load(object sender, EventArgs e)
        {
            tWidth.Text = Bundle_Width.ToString();
            tSideFrameTHK.Text = SideFrame_THK.ToString();

            createDrawing_Toggle.Checked = Default.Toggle_CreateDrawing;
            save_Toggle.Checked = Default.Toggle_Save;
            delete_Toggle.Checked = Default.Toggle_DeleteFiles;
        }

        private void tWidth_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tWidth.Text, x => Bundle_Width = x);
        }

        private void tSideFrameTHK_Leave(object sender, EventArgs e)
        {
            UI_DoubleChanged(tSideFrameTHK.Text, x => SideFrame_THK = x);
            tSideFrameTHK.Text = SideFrame_THK.ToString();
        }

        private void bBundle_Click(object sender, EventArgs e)
        {
            new Bundle(7, "Bundle");
        }

        private void createDrawing_Toggle_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(createDrawing_Toggle.Checked, x => Default.Toggle_CreateDrawing = x);
        }

        private void save_Toggle_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(save_Toggle.Checked, x => Default.Toggle_Save = x);
        }

        private void delete_Toggle_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(delete_Toggle.Checked, x => Default.Toggle_DeleteFiles = x);
        }
    }
}
