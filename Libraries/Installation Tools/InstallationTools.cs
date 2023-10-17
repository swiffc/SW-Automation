using EPDM.Interop.epdm;
using System.Diagnostics;

namespace Installation_Tools
{
    public class InstallationTools
    {
        public string WorkingDirectory = 
            @"C:\Windows\Microsoft.NET\Framework64\v4.0.30319";
        public string CMD = 
            @"C:\windows\system32\cmd.exe";
        public string DllLocation =
            "\"C:\\AXC_VAULT\\Active\\_Automation Tools\\Hudson_\\Drafting\\Automation\\Solidworks Add-In\\SolidWorks Add-In.dll\"";

        public static void Install()
        {
            InstallationTools x = new InstallationTools();
            var processStartInfo = new ProcessStartInfo
            {
                WorkingDirectory = x.WorkingDirectory,
                FileName = x.CMD,
                Arguments =
                "/K " +
                "RegAsm.exe " +
                "/codebase " +
                $"{x.DllLocation}"
            };
            Process proc = Process.Start(processStartInfo);
        }

        public static void Uninstall()
        {
            InstallationTools x = new InstallationTools();
            var processStartInfo = new ProcessStartInfo
            {
                WorkingDirectory = x.WorkingDirectory,
                FileName = x.CMD,
                Arguments =
                "/K " +
                "RegAsm.exe " +
                $"{x.DllLocation} " +
                "/u"
            };
            Process proc = Process.Start(processStartInfo);
        }

        public static void SilentUninstall()
        {
            InstallationTools x = new InstallationTools();
            var processStartInfo = new ProcessStartInfo
            {
                WorkingDirectory = x.WorkingDirectory,
                FileName = x.CMD,
                Arguments =
                "/C " +
                "RegAsm.exe " +
                $"{x.DllLocation} " +
                "/u"
            };
            Process proc = Process.Start(processStartInfo);
        }

        public static void KillSolidworks()
        {
            try
            {
                foreach (var process in Process.GetProcessesByName("SLDWORKS"))
                {
                    process.Kill();
                }
            }
            catch (System.Exception) { }
        }

        public static void GetLatestAddin()
        {
            IEdmVault5 vault = new EdmVault5();
            vault.LoginAuto("AXC_VAULT", 1);

            IEdmFolder5 folder = vault.RootFolder;
            folder = folder.GetSubFolder("Active");
            folder = folder.GetSubFolder("_Automation Tools");
            folder = folder.GetSubFolder("Hudson_");
            folder = folder.GetSubFolder("Drafting");
            folder = folder.GetSubFolder("Automation");
            folder = folder.GetSubFolder("Solidworks Add-In");

            IEdmFile5 file = folder.GetFile("logo-small.bmp");
            file.GetFileCopy
                (
                0,
                null,
                null,
                1,
                null
                );

            file = folder.GetFile("SolidWorks Add-in.dll");
            file.GetFileCopy
                (
                0,
                null,
                null,
                1,
                null
                );

            file = folder.GetFile("SolidWorks Automation Library.dll");
            file.GetFileCopy
                (
                0,
                null,
                null,
                1,
                null
                );


        }

        public static void GetLatestInstaller()
        {
            IEdmVault5 vault = new EdmVault5();
            vault.LoginAuto("AXC_VAULT", 1);
            
            IEdmFolder5 folder = vault.RootFolder;
            folder = folder.GetSubFolder("Active");
            folder = folder.GetSubFolder("_Automation Tools");
            folder = folder.GetSubFolder("Hudson_");
            folder = folder.GetSubFolder("Drafting");
            folder = folder.GetSubFolder("Automation");
            folder = folder.GetSubFolder("Add-In Installer");

            IEdmFile5 file = folder.GetFile("InstallationTools.dll");
            file.GetFileCopy
                (
                0,
                null,
                null,
                1,
                null
                );
        }
    }

}
