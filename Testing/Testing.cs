using EPDM.Interop.epdm;
using System;
using System.Collections.Generic;
using System.Windows.Forms;
using SolidWorks.Interop.sldworks;

namespace Testing
{
    internal class Testing
    {
        static void Main()
        {
            EdmVault5 vault = new EdmVault5();
            try
            {
                vault.LoginAuto("AXC_VAULT", 0);
            }
            catch (Exception ex)
            {
                MessageBox.Show("Failed to connect to the vault: " + ex.Message, "Connection Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                return;
            }

            IEdmFile5 updaterFile = vault.GetFileFromPath(@"C:\Solidworks Automation\Solidworks Add-In\bin\Debug\AddInUpdater.exe", out IEdmFolder5 updaterFolder);
            updaterFile.GetFileCopy(0);
        }
    }
}
