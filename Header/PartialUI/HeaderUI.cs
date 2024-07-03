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
using Excel;
using static FileTools.StaticFileTools;
using static Excel.Prego;
using System.Runtime.InteropServices;

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
            Prego.CleanUp();
        }
        private void job_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(job_Box.Text, x => Project = x);
            Prego.CleanUp();
        }
        private void textBox_Bank_TextChanged(object sender, EventArgs e)
        {
            UI_CharChanged(textBox_Bank.Text, x => Bank = x);
            Prego.CleanUp();
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

            CleanUp();
        }
        private void bCreateUpdate_Click(object sender, EventArgs e)
        {
            if (Header61.IsRequired)
            {
                HeaderBase.Header = Header61;
                new HeaderBase(61, "Header");
            }    
        }
        private void test62_Click(object sender, EventArgs e)
        {
            if (Header62.IsRequired)
            {
                HeaderBase.Header = Header62;
                new HeaderBase(61, "Header");
            }
        }

        private void test63_Click(object sender, EventArgs e)
        {
            if (Header63.IsRequired)
            {
                HeaderBase.Header = Header63;
                new HeaderBase(61, "Header");
            }
        }

        private void test64_Click(object sender, EventArgs e)
        {
            if (Header64.IsRequired)
            {
                HeaderBase.Header = Header64;
                new HeaderBase(61, "Header");
            }
        }

        private void test65_Click(object sender, EventArgs e)
        {
            if (Header65.IsRequired)
            {
                HeaderBase.Header = Header65;
                new HeaderBase(61, "Header");
            }
        }

        private void test66_Click(object sender, EventArgs e)
        {
            if (Header66.IsRequired)
            {
                HeaderBase.Header = Header66;
                new HeaderBase(61, "Header");
            }
        }
    }
}
