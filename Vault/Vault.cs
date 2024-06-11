using EPDM.Interop.epdm;
using System;
using System.Collections.Generic;
using System.Windows.Forms;

namespace AXC_Vault
{
    public static class Vault
    {
        static private EdmVault5 _vault5;
        public static EdmVault5 Vault5
        {
            get 
            { 
                if (_vault5 == null)
                {
                    _vault5 = new EdmVault5();
                    try
                    {
                        _vault5.LoginAuto("AXC_VAULT", 0);
                        return _vault5;
                    }
                    catch (Exception ex)
                    {
                        MessageBox.Show("Failed to connect to the vault: " + ex.Message, "Connection Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                        return null;
                    }
                }
                return _vault5;
            }
        }
        public static void GetAllFilesInFolderAndSubFolders(IEdmFolder5 folder)
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
        public static void GetAllFilesInFolderAndSubFolders(string folderPath)
        {
            IEdmFolder5 folder = Vault5.GetFolderFromPath(folderPath);
            GetAllFilesInFolderAndSubFolders(folder);
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
        public static void GetAllFilesInFolder(string folderPath)
        {
            IEdmFolder5 folder = Vault5.GetFolderFromPath(folderPath);   
            GetAllFilesInFolder(folder, false);
        }
        private static List<string> FoldersToSkip = null;
        public static bool FileExists(string filePath, out IEdmFile5 file)
        {
            file = Vault5.GetFileFromPath(filePath, out _);
            return file != null ?  true : false;
        }
        public static void DownloadFile(IEdmFile5 file)
        {
            file.GetFileCopy(0);
        }
    }
    
}
