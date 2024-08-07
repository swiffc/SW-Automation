using EPDM.Interop.epdm;
using System;
using System.Collections.Generic;
using System.Windows.Forms;
using SolidWorks.Interop.sldworks;
using System.Linq;
using AXC_Vault;
using static AXC_Vault.Vault;

namespace AddInUpdater
{
    public class AddInUpdater
    {
        public static void Main()
        {
            UpdateAddIn();
        }

        private static List<string> FoldersToSkip = new List<string>
        {
            @"C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting\Certified\Hood\1976 Hood Program",
            //@"C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting\Certified\zRAGU - Design Table Automation",
            @"C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting\Automation\Solidworks Add-In",
            @"C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting\Automation\Add-In Installer",

            @"C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting\Certified\MachineryMount"
        };

        public static void UpdateAddIn(bool restartSolidworks = false)
        {
            // Retrieve a reference to the specified folder within the vault
            IEdmFolder5 folder = Vault5.GetFolderFromPath(@"C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting");

            // Recursively retrieve and list all files from the specified folder and its subfolders
            GetAllFilesInFolderAndSubFolders(folder);

            if (restartSolidworks)
            {
                SldWorks sldWorks = new SldWorks();
                sldWorks.Visible = true;
            }

            Console.WriteLine("\n\n\n" + "Press any key to close");
            Console.Read();
        }
    }
}
