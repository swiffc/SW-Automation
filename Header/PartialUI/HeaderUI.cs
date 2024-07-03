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
            // Automated
            MapLocal_UI_To_DTO(this);
            LoadHeaderData_FromApp("61");
            LoadHeaderData_FromApp("62");
            LoadHeaderData_FromApp("63");
            LoadHeaderData_FromApp("64");
            LoadHeaderData_FromApp("65");
            LoadHeaderData_FromApp("66");

            // Manual
            cIsBusted61.Checked = Header61.IsBusted;
            cIsBusted62.Checked = Header62.IsBusted;
            cIsBusted63.Checked = Header63.IsBusted;
            cIsBusted64.Checked = Header64.IsBusted;
            cIsBusted65.Checked = Header65.IsBusted;
            cIsBusted66.Checked = Header66.IsBusted;

            cIsBusted61.Enabled = Header61.IsRequired;
            cIsBusted62.Enabled = Header62.IsRequired;
            cIsBusted63.Enabled = Header63.IsRequired;
            cIsBusted64.Enabled = Header64.IsRequired;
            cIsBusted65.Enabled = Header65.IsRequired;
            cIsBusted66.Enabled = Header66.IsRequired;
        }
        private void HeaderUI_Load(object sender, EventArgs e)
        {
            job_Box.Text = Project;
            textBox_Bank.Text = Bank.ToString();

            LoadAllHeaderData();
        }
        private void ImportHeaderData_FromPrego_Manual()
        {
            // Get bust boolean by cell
            LoadPregoBool(cIsBusted61, InputsCalcsSheet, "AAI45");
            LoadPregoBool(cIsBusted63, InputsCalcsSheet, "AAJ45");
            LoadPregoBool(cIsBusted65, InputsCalcsSheet, "AAK45");
            LoadPregoBool(cIsBusted62, InputsCalcsSheet, "ADC45");
            LoadPregoBool(cIsBusted64, InputsCalcsSheet, "ADD45");
            LoadPregoBool(cIsBusted66, InputsCalcsSheet, "ADE45");
        }
        #region Buttons
        private void bImportPrego_Click(object sender, EventArgs e)
        {
            // Automated imports Double values from Prego
            ImportHeaderData_FromPrego();

            // Manual imports for non-Double values
            ImportHeaderData_FromPrego_Manual();

            // Excel
            CleanUp();
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
        #endregion
        #region Manual Event Handlers
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

            cIsBusted61.Enabled = Header61.IsRequired;
        }
        private void cEnabled62_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled62.Checked, x => Header62.IsRequired = x);
            LoadHeaderData_FromApp("62");

            cIsBusted62.Enabled = Header62.IsRequired;
        }
        private void cEnabled63_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled63.Checked, x => Header63.IsRequired = x);
            LoadHeaderData_FromApp("63");

            cIsBusted63.Enabled = Header63.IsRequired;
        }
        private void cEnabled64_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled64.Checked, x => Header64.IsRequired = x);
            LoadHeaderData_FromApp("64");

            cIsBusted64.Enabled = Header64.IsRequired;
        }
        private void cEnabled65_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled65.Checked, x => Header65.IsRequired = x);
            LoadHeaderData_FromApp("65");

            cIsBusted65.Enabled = Header65.IsRequired;
        }
        private void cEnabled66_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled66.Checked, x => Header66.IsRequired = x);
            LoadHeaderData_FromApp("66");

            cIsBusted66.Enabled = Header66.IsRequired;
        }
        private void cIsBusted61_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cIsBusted61.Checked, x => Header61.IsBusted = x);
        }

        private void cIsBusted62_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cIsBusted62.Checked, x => Header62.IsBusted = x);
        }

        private void cIsBusted63_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cIsBusted63.Checked, x => Header63.IsBusted = x);
        }

        private void cIsBusted64_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cIsBusted64.Checked, x => Header64.IsBusted = x);
        }

        private void cIsBusted65_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cIsBusted65.Checked, x => Header65.IsBusted = x);
        }

        private void cIsBusted66_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cIsBusted66.Checked, x => Header66.IsBusted = x);
        }
        #endregion 
    }
}
