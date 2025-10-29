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
            @"C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\templates\hudson_certified\Hood\1976 Hood Program",
            //@"C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\templates\hudson_certified\zRAGU - Design Table Automation",
            @"C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation",
            @"C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation\Solidworks Add-In",

            @"C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\templates\hudson_certified\MachineryMount"
        };

        public static void UpdateAddIn(bool restartSolidworks = false)
        {
            // Retrieve a reference to the specified folder within the vault
            IEdmFolder5 folder = Vault5.GetFolderFromPath(@"C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\templates\hudson_certified");

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
