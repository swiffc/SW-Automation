using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using SolidWorks.Interop.sldworks;
using System.Windows.Forms;
using System.Windows;
using EPDM.Interop.epdm;
using System.Threading;
using static AddInUpdater.AddInUpdater;

namespace AddInDllVersionControl
{
    internal class AddInDllVersionControl
    {
        static void Main()
        {
            string regasmPath = @"C:\Windows\Microsoft.NET\Framework64\v4.0.30319\RegAsm.exe";
            string dllPath = @"C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting\Automation\Solidworks Add-In\Automation Guy.dll";
            string updaterPath = @"C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting\Automation\Add-In Updater\AddInUpdater.exe";

            // Close Solidworks
            foreach (Process process in Process.GetProcesses())
            {
                if (process.ProcessName == "SLDWORKS")
                {
                    try
                    {
                        object swInstance = Marshal.GetActiveObject("SldWorks.Application");
                        SldWorks swApp = (SldWorks)swInstance;

                        MessageBox.Show("SolidWorks is about to close. " + "\n" +
                                        "Please ensure all work is saved before pressing OK to continue.",
                                        "Warning", MessageBoxButtons.OK, MessageBoxIcon.Warning);

                        // Close all documents without saving
                        swApp.CloseAllDocuments(false);

                        // Close SolidWorks
                        swApp.ExitApp();
                    }
                    catch (Exception ex)
                    {
                        Console.WriteLine("An exception occured: " + ex.Message);
                    }
                }
            }

            // Login
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

            Console.WriteLine("Updating Automation Guy add-in...");

            // Unregister the DLL
            RunRegasm(regasmPath, dllPath, true);

            // Update DLL
            IEdmFile5 file = vault.GetFileFromPath(dllPath, out IEdmFolder5 dllFolder);
            file.GetFileCopy(0);
            if (file != null)
            {
                Console.WriteLine($"Successfully updated {dllPath} to the current version");
            }
            else
            {
                Console.WriteLine($"Could not update {dllPath} to the current version");
            }

            // Update updater
            IEdmFile5 updaterFile = vault.GetFileFromPath(updaterPath, out IEdmFolder5 updaterFolder);
            updaterFile.GetFileCopy(0);

            // Update add-in dependencies
            GetAllFilesInFolder(dllFolder, false);

            // Register the DLL
            RunRegasm(regasmPath, dllPath, false);

            // Relaunch Solidworks
            try
            {
                // Path to the SolidWorks executable
                string solidWorksExePath = @"C:\Program Files\SOLIDWORKS Corp\SOLIDWORKS\SLDWORKS.exe";

                // Start SolidWorks
                Process.Start(solidWorksExePath);
            }
            catch (Exception ex)
            {
                Console.WriteLine("Error starting SolidWorks: " + ex.Message);
            }

            Console.WriteLine("\n\n\n" + "Press any key to close");
            Console.Read();
        }

        private static void RunRegasm(string regasmPath, string dllPath, bool unregister)
        {
            var process = new Process
            {
                StartInfo = new ProcessStartInfo
                {
                    FileName = regasmPath,
                    Arguments = $"/codebase \"{dllPath}\"",
                    Verb = "runas", // Run as admin
                    RedirectStandardInput = true, // Redirect streams
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    UseShellExecute = false,
                    CreateNoWindow = true
                }
            };

            // Start the process and read its output
            process.Start();
            var result = process.StandardError.ReadToEnd();
            process.WaitForExit();

            if (process.ExitCode == 0)
            {
                Console.WriteLine($"Successfully {(unregister ? "unregistered" : "registered")} {dllPath}");
            }
            else
            {
                Console.WriteLine("Unexpected response");

            }


        }
    }
}
