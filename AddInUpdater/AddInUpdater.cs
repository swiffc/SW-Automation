using EPDM.Interop.epdm;
using System;
using System.Collections.Generic;
using System.Windows.Forms;
using SolidWorks.Interop.sldworks;
using System.Linq;

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
            @"C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting\Certified\zRAGU - Design Table Automation",
            @"C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting\Automation\Solidworks Add-In",
            @"C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting\Automation\Add-In Installer",
        };

        public static void UpdateAddIn(bool restartSolidworks = false)
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

            // Retrieve a reference to the specified folder within the vault
            IEdmFolder5 folder = vault.GetFolderFromPath(@"C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting");

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

        private static void GetAllFilesInFolderAndSubFolders(IEdmFolder5 folder)
        {
            // Process all files in the current folder
            GetAllFilesInFolder(folder, true);

            // Get the position of the first subfolder
            IEdmPos5 subFolderPos = folder.GetFirstSubFolderPosition();
            while (!subFolderPos.IsNull)
            {
                // Retrieve the subfolder at the current position
                IEdmFolder5 subFolder = folder.GetNextSubFolder(subFolderPos);
                if (subFolder != null && !FoldersToSkip.Contains(subFolder.LocalPath))
                {
                    // Recursively process files in this subfolder
                    GetAllFilesInFolderAndSubFolders(subFolder);
                }
            }
        }

        public static void GetAllFilesInFolder(IEdmFolder5 folder, bool skipCheckedOutFiles)
        {
            Console.WriteLine("\n" + $"Checking all files in {folder.Name}");

            // Get the position of the first file in the folder
            IEdmPos5 filePosition = folder.GetFirstFilePosition();

            while (filePosition != null && !filePosition.IsNull)
            {
                // Retrieve the file at the current position
                IEdmFile5 file = folder.GetNextFile(filePosition);
                if (file != null)
                {
                    // Check if the file is checked out
                    if (!file.IsLocked || !skipCheckedOutFiles)
                    {
                        // Cast to IEdmFile12 to access GetLocalVersionNo2
                        IEdmFile12 file12 = file as IEdmFile12;
                        if (file12 != null)
                        {
                            bool localVersionObsolete;
                            object filePath = file.GetLocalPath(folder.ID); // Use the local path of the file
                            int localVersionNo = file12.GetLocalVersionNo2(ref filePath, out localVersionObsolete);
                            int currentVersion = file.CurrentVersion;

                            // Check if the local version is older than the current version
                            if (localVersionNo < currentVersion || localVersionObsolete)
                            {
                                // If so, get a copy of the latest file version
                                file.GetFileCopy(0);
                                Console.WriteLine($"  Updated {file.Name} to version {currentVersion}.");
                            }
                            else
                            {
                                Console.WriteLine($"  {file.Name} is up to date.");
                            }
                        }
                        else
                        {
                            Console.WriteLine($"  Unable to cast file '{file.Name}' to IEdmFile12.");
                        }
                    }
                    else
                    {
                        Console.WriteLine($"  {file.Name} is checked out and will be skipped.");
                    }
                }
                else
                {
                    // If the file could not be retrieved, output an error message
                    Console.WriteLine("Failed to get the file.");
                }
            }
        }
    }
}
