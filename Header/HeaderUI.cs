using System;
using System.Windows.Forms;
using static Excel.StaticHelpers;
using static FileTools.CommonData.CommonData;

namespace HDR
{
    public partial class HeaderUI : Form
    {
        public HeaderUI()
        {
            InitializeComponent();

            #region HeaderData Event Handlers Description
            // HeaderData event handlers
            //    To use:
            //       1) Header_DataManager.Prego_DTO
            //       2) Header_DataManager.UI_DTO
            //       3) Header_DataManager.MapLocal_UI_To_DTO
            //       4) Header_DataManager._headerPregoData
            //       5) Settings.settings
            //       6) IHeaderExtensions.cs
            //             TextBox.Name must equal:
            //                t{explicitPropertyName}{implicitPropertyName}_{headerNo}
            //                   example:
            //                      t{Tubesheet}{THK}_{61} --> tTubesheetTHK_61
            #endregion
            Header_TextChanged(this, "Box");
            Header_TextChanged(this, "Tubesheet");
            Header_TextChanged(this, "Plugsheet");
            Header_TextChanged(this, "TopBtm");
            Header_TextChanged(this, "EndPlate");
            Header_TextChanged(this, "Tube");

        }

        private void HeaderUI_Load(object sender, EventArgs e)
        {
            job_Box.Text = Project;
            textBox_Bank.Text = Bank.ToString();

            LoadAllHeaderData();
        }
    }
}
