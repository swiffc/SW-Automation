using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using static FileTools.CommonData.CommonData;
using static Excel.Header_DataManager;
using FileTools.Base;
using System.Reflection;
using SplashScreen;
using static Excel.StaticHelpers;

namespace HDR
{
    public partial class HeaderUI
    {
        private void LoadAllHeaderData()
        {
            MapLocal_UI_To_DTO(this);
            LoadHeaderData_FromApp("61");
            LoadHeaderData_FromApp("62");
            LoadHeaderData_FromApp("63");
            LoadHeaderData_FromApp("64");
            LoadHeaderData_FromApp("65");
            LoadHeaderData_FromApp("66");
        }
        private void HeaderUI_FormClosing(object sender, FormClosingEventArgs e)
        {
            try
            {
                PleaseWait.Stop();
            }
            catch (Exception) { }
        }
        private void job_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(job_Box.Text, x => Project = x);
        }
        private void textBox_Bank_TextChanged(object sender, EventArgs e)
        {
            UI_CharChanged(textBox_Bank.Text, x => Bank = x);
        }
        private void cEnabled61_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled61.Checked, x => Header61.IsRequired = x);
            LoadHeaderData_FromApp("61");
        }
        private void cEnabled62_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled62.Checked, x => Header62.IsRequired = x);
            LoadHeaderData_FromApp("62");
        }
        private void cEnabled63_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled63.Checked, x => Header63.IsRequired = x);
            LoadHeaderData_FromApp("63");
        }
        private void cEnabled64_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled64.Checked, x => Header64.IsRequired = x);
            LoadHeaderData_FromApp("64"); ;
        }
        private void cEnabled65_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled65.Checked, x => Header65.IsRequired = x);
            LoadHeaderData_FromApp("65");
        }
        private void cEnabled66_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled66.Checked, x => Header66.IsRequired = x);
            LoadHeaderData_FromApp("66");
        }
        private void bImportPrego_Click(object sender, EventArgs e)
        {
            ImportHeaderData_FromPrego();
        }
        private void bCreateUpdate_Click(object sender, EventArgs e)
        {
            if (Header61.IsRequired)
            {
                HeaderBase.Header = Header61;
                new HeaderBase(61, "Header");
            }
                
        }
    }
}
