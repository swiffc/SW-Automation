using Excel;
using FileTools.Base;
using FileTools.CommonData.Headers.Connections;
using SplashScreen;
using System;
using System.Collections.Generic;
using System.Windows.Forms;
using static Excel.Prego;
using static Excel.StaticHelpers;
using static FileTools.CommonData.CommonData;
using static FileTools.Properties.Settings;

namespace HDR
{
    public partial class HeaderUI
    {
        private void LoadAllHeaderData()
        {
            // Automated
            Header_DataManager.MapLocal_UI_To_DTO(this);
            Header_DataManager.LoadHeaderData_FromApp("61");
            Header_DataManager.LoadHeaderData_FromApp("62");
            Header_DataManager.LoadHeaderData_FromApp("63");
            Header_DataManager.LoadHeaderData_FromApp("64");
            Header_DataManager.LoadHeaderData_FromApp("65");
            Header_DataManager.LoadHeaderData_FromApp("66");

            Connection_DataManager.MapLocal_UI_To_DTO(this);
            Connection_DataManager.LoadConnectionData_FromApp("Inlet");
            Connection_DataManager.LoadConnectionData_FromApp("Outlet");

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

            cExtensionType_Inlet.Text = Inlet.ExtensionType;
            cExtensionType_Outlet.Text = Outlet.ExtensionType;

            cTileblockManuf.Text = TitleblockManuf;
            cHeadersOutsideFrames.Checked = HeadersOutsideFrames;

        }
        private void HeaderUI_Load(object sender, EventArgs e)
        {
            job_Box.Text = Project;
            textBox_Bank.Text = Bank.ToString();

            LoadAllHeaderData();
        }
        private void ImportHeaderData_FromPrego_Manual()
        {
            LoadPregoBool(cIsBusted61, InputsCalcsSheet, "AAI45");
            LoadPregoBool(cIsBusted63, InputsCalcsSheet, "AAJ45");
            LoadPregoBool(cIsBusted65, InputsCalcsSheet, "AAK45");
            LoadPregoBool(cIsBusted62, InputsCalcsSheet, "ADC45");
            LoadPregoBool(cIsBusted64, InputsCalcsSheet, "ADD45");
            LoadPregoBool(cIsBusted66, InputsCalcsSheet, "ADE45");

            ImportBustedSpans();

            LoadPregoString(cExtensionType_Inlet, InputsCalcsSheet, "CV17");
            LoadPregoString(cExtensionType_Outlet, InputsCalcsSheet, "DX18");

            LoadPregoString(cTileblockManuf, InputsCalcsSheet, "R49");
            LoadPregoBool(cHeadersOutsideFrames, InputsCalcsSheet, "R41");

            ImportPartStiffPartNumbers();
            ImportConnectionPartNumbers();

            ConnectionPartNoSafety();

            var material = CellString(InputsCalcsSheet, "C3");
            if (material == "Carbon Steel")
            {
                Default.Stainless = false;
                cSS.Checked = false;
            }
            else
            {
                Default.Stainless = true;
                cSS.Checked = true;
            }
            SaveSettings();
        }
        void ConnectionPartNoSafety()
        {
            if (tFlangePartNo_Inlet.Text.Length <= 1)
            {
                string value = "InletFlange";
                tFlangePartNo_Inlet.Text = value;
                Inlet.FlangePartNo = value;
            }
            if (tFlangePartNo_Outlet.Text.Length <= 1)
            {
                string value = "OutletFlange";
                tFlangePartNo_Outlet.Text = value;
                Outlet.FlangePartNo = value;
            }
            if (tExtensionPartNo_Inlet.Text.Length <= 1)
            {
                string value = "InletExtension";
                tExtensionPartNo_Inlet.Text = value;
                Inlet.ExtensionPartNo = value;
            }
            if (tExtensionPartNo_Outlet.Text.Length <= 1)
            {
                string value = "OutletExtension";
                tExtensionPartNo_Outlet.Text = value;
                Outlet.ExtensionPartNo = value;
            }
        }
        private void ImportConnectionPartNumbers()
        {
            // Inlet flange
            string lookupCell = InletFlangePartNumberCell(Inlet.Location);
            LoadPregoString(tFlangePartNo_Inlet, BomInputSheet, lookupCell);

            // Outlet flange
            lookupCell = OutletFlangePartNumberCell(Outlet.Location);
            LoadPregoString(tFlangePartNo_Outlet, BomInputSheet, lookupCell);

            // Inlet extension
            lookupCell = InletExtensionPartNumberCell(Inlet.Location);
            LoadPregoString(tExtensionPartNo_Inlet, BomInputSheet, lookupCell);

            // Outlet extension
            lookupCell = OutletExtensionPartNumberCell(Outlet.Location);
            LoadPregoString(tExtensionPartNo_Outlet, BomInputSheet, lookupCell);
        }
        private string InletExtensionPartNumberCell(string location)
        {
            int headerNo = DetermineHeaderNumber(location);
            switch (headerNo)
            {
                case 61:
                    return "LJ114";
                case 63:
                    return "LJ372";
                case 65:
                    return "LJ630";
                case 62:
                    return "LJ888";
                case 64:
                    return "LJ1146";
                case 66:
                    return "LJ1404";
                default:
                    throw new Exception("Invalid header number");
            }
        }
        private string OutletExtensionPartNumberCell(string location)
        {
            int headerNo = DetermineHeaderNumber(location);
            switch (headerNo)
            {
                case 61:
                    return "LJ130";
                case 63:
                    return "LJ388";
                case 65:
                    return "LJ646";
                case 62:
                    return "LJ904";
                case 64:
                    return "LJ1162";
                case 66:
                    return "LJ1420";
                default:
                    throw new Exception("Invalid header number");
            }
        }   
        private string OutletFlangePartNumberCell(string location)
        {
            int headerNo = DetermineHeaderNumber(location);
            switch (headerNo)
            {
                case 61:
                    return "LJ126";
                case 63:    
                    return "LJ384";
                case 65:    
                    return "LJ642";
                case 62:    
                    return "LJ900";
                case 64:   
                    return "LJ1158";
                case 66:    
                    return "LJ1416";
                default:
                    throw new Exception("Invalid header number");
            }
        }
        private string InletFlangePartNumberCell(string location)
        {
            int headerNo = DetermineHeaderNumber(location);
            switch (headerNo)
            {
                case 61:
                    return "LJ110";
                case 63:
                    return "LJ368";
                case 65:
                    return "LJ626";
                case 62:
                    return "LJ884";
                case 64:
                    return "LJ1142";
                case 66:
                    return "LJ1400";
                default:
                    throw new Exception("Invalid header number");
            }
        }
        private int DetermineHeaderNumber(string location)
        {
            switch (location)
            {
                case "TL":
                    return 61;
                case "TR":
                    return 62;
                case "BL":
                    return LowestLeftHeader();
                case "BR":
                    return LowestRightHeader();
                default:
                    throw new Exception("Invalid location");
            }
        }
        private int LowestLeftHeader()
        {
            if (Header65.IsRequired)
                return 65;
            else if (Header63.IsRequired)
                return 63;
            else 
                return 61;
        }
        private int LowestRightHeader()
        {
            if (Header66.IsRequired)
                return 66;
            else if (Header64.IsRequired)
                return 64;
            else
                return 62;
        }

        private void ImportBustedSpans()
        {
            List<double> bustSpans_61 = CellDoubleList(InputsCalcsSheet, CellNameColumnArray("AAI5", "AAI43"));
            AssignSpans(value => Default.EndPlateBustedSpan2_61 = value, tEndPlateBustedSpan2_61, 2, bustSpans_61);

            List<double> bustSpans_62 = CellDoubleList(InputsCalcsSheet, CellNameColumnArray("ADC5", "ADC43"));
            AssignSpans(value => Default.EndPlateBustedSpan2_62 = value, tEndPlateBustedSpan2_62, 2, bustSpans_62);

            List<double> bustSpans_63 = CellDoubleList(InputsCalcsSheet, CellNameColumnArray("AAJ5", "AAJ43"));
            AssignSpans(value => Default.EndPlateBustedSpan2_63 = value, tEndPlateBustedSpan2_63, 2, bustSpans_63);

            List<double> bustSpans_64 = CellDoubleList(InputsCalcsSheet, CellNameColumnArray("ADD5", "ADD43"));
            AssignSpans(value => Default.EndPlateBustedSpan2_64 = value, tEndPlateBustedSpan2_64, 2, bustSpans_64);

            List<double> bustSpans_65 = CellDoubleList(InputsCalcsSheet, CellNameColumnArray("AAK5", "AAK43"));
            AssignSpans(value => Default.EndPlateBustedSpan2_65 = value, tEndPlateBustedSpan2_65, 2, bustSpans_65);

            List<double> bustSpans_66 = CellDoubleList(InputsCalcsSheet, CellNameColumnArray("ADE5", "ADE43"));
            AssignSpans(value => Default.EndPlateBustedSpan2_66 = value, tEndPlateBustedSpan2_66, 2, bustSpans_66);

        }

        #region Buttons
        private void bImportPrego_Click(object sender, EventArgs e)
        {
            if (PregoDoc != null)
            {
                // Automated imports Double values from Prego
                Header_DataManager.ImportHeaderData_FromPrego();
                Connection_DataManager.ImportConnectionData_FromPrego();

                // Manual imports for non-Double values
                ImportHeaderData_FromPrego_Manual();

                // Excel
                CleanUp();

                MessageBox.Show($"Data imported from Prego successfully", "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
            else
            {
                MessageBox.Show("Prego file not found", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }

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
        private bool CheckIfFlangeAndExtensionPartNumbers(IConnection connection)
        {
            string connectionName = connection.GetType().Name;
            if (connection.FlangePartNo == connection.ExtensionPartNo)
            {
                MessageBox.Show($"{connectionName} Flange and {connectionName} Extension part numbers may not be the same", "PartNo Conflict", MessageBoxButtons.OK, MessageBoxIcon.Error);
                return true;
            }
            return false;
        }
        private bool CheckFlangeAndExtensionPartNumbers()
        {
            bool checkInlet = CheckIfFlangeAndExtensionPartNumbers(Inlet);
            bool checkOutlet = CheckIfFlangeAndExtensionPartNumbers(Outlet);
            return checkInlet || checkOutlet;
        }
        private void bRun_Click(object sender, EventArgs e)
        {
            if (CheckFlangeAndExtensionPartNumbers()) return;

            if (Header61.IsRequired)
            {
                HeaderBase.Header = Header61;
                new HeaderBase(61, "Header");
            }
            if (Header62.IsRequired)
            {
                HeaderBase.Header = Header62;
                new HeaderBase(62, "Header");
            }
            if (Header63.IsRequired)
            {
                HeaderBase.Header = Header63;
                new HeaderBase(63, "Header");
            }
            if (Header64.IsRequired)
            {
                HeaderBase.Header = Header64;
                new HeaderBase(64, "Header");
            }
            if (Header65.IsRequired)
            {
                HeaderBase.Header = Header65;
                new HeaderBase(65, "Header");
            }
            if (Header66.IsRequired)
            {
                HeaderBase.Header = Header66;
                new HeaderBase(66, "Header");
            }
        }
        private void bCreateUpdate_Click(object sender, EventArgs e)
        {
            if (CheckFlangeAndExtensionPartNumbers()) return;
            if (Header61.IsRequired)
            {
                HeaderBase.Header = Header61;
                new HeaderBase(61, "Header");
            }
        }
        private void test62_Click(object sender, EventArgs e)
        {
            if (CheckFlangeAndExtensionPartNumbers()) return;
            if (Header62.IsRequired)
            {
                HeaderBase.Header = Header62;
                new HeaderBase(62, "Header");
            }
        }
        private void test63_Click(object sender, EventArgs e)
        {
            if (CheckFlangeAndExtensionPartNumbers()) return;
            if (Header63.IsRequired)
            {
                HeaderBase.Header = Header63;
                new HeaderBase(63, "Header");
            }
        }
        private void test64_Click(object sender, EventArgs e)
        {
            if (CheckFlangeAndExtensionPartNumbers()) return;
            if (Header64.IsRequired)
            {
                HeaderBase.Header = Header64;
                new HeaderBase(64, "Header");
            }
        }
        private void test65_Click(object sender, EventArgs e)
        {
            if (CheckFlangeAndExtensionPartNumbers()) return;
            if (Header65.IsRequired)
            {
                HeaderBase.Header = Header65;
                new HeaderBase(65, "Header");
            }
        }
        private void test66_Click(object sender, EventArgs e)
        {
            if (CheckFlangeAndExtensionPartNumbers()) return;
            if (Header66.IsRequired)
            {
                HeaderBase.Header = Header66;
                new HeaderBase(66, "Header");
            }
        }
        #endregion

        #region Manual Event Handlers
        private void cTileblockManuf_SelectedIndexChanged(object sender, EventArgs e)
        {
            UI_StringChanged(cTileblockManuf.Text, x => TitleblockManuf = x);
        }
        private void cHeadersOutsideFrames_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cHeadersOutsideFrames.Checked, x => HeadersOutsideFrames = x);
        }
        private void tExtensionType_Inlet_SelectedIndexChanged(object sender, EventArgs e)
        {
            UI_StringChanged(cExtensionType_Inlet.Text, x => Inlet.ExtensionType = x);
        }
        private void tExtensionType_Outlet_SelectedIndexChanged(object sender, EventArgs e)
        {
            UI_StringChanged(cExtensionType_Outlet.Text, x => Outlet.ExtensionType = x);
        }
        private void cLocation_Inlet_SelectedIndexChanged(object sender, EventArgs e)
        {
            UI_StringChanged(cLocation_Inlet.Text, x => Inlet.Location = x);
        }
        private void cLocation_Outlet_SelectedIndexChanged(object sender, EventArgs e)
        {
            UI_StringChanged(cLocation_Outlet.Text, x => Outlet.Location = x);
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
            Header_DataManager.LoadHeaderData_FromApp("61");

            cIsBusted61.Enabled = Header61.IsRequired;
        }
        private void cEnabled62_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled62.Checked, x => Header62.IsRequired = x);
            Header_DataManager.LoadHeaderData_FromApp("62");

            cIsBusted62.Enabled = Header62.IsRequired;
        }
        private void cEnabled63_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled63.Checked, x => Header63.IsRequired = x);
            Header_DataManager.LoadHeaderData_FromApp("63");

            cIsBusted63.Enabled = Header63.IsRequired;
        }
        private void cEnabled64_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled64.Checked, x => Header64.IsRequired = x);
            Header_DataManager.LoadHeaderData_FromApp("64");

            cIsBusted64.Enabled = Header64.IsRequired;
        }
        private void cEnabled65_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled65.Checked, x => Header65.IsRequired = x);
            Header_DataManager.LoadHeaderData_FromApp("65");

            cIsBusted65.Enabled = Header65.IsRequired;
        }
        private void cEnabled66_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled66.Checked, x => Header66.IsRequired = x);
            Header_DataManager.LoadHeaderData_FromApp("66");

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
