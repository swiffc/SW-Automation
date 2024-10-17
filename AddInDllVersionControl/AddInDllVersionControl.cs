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
using AXC_Vault;
using static AXC_Vault.Vault;

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
            Console.WriteLine("Updating Automation Guy add-in...");

            // Unregister the DLL
            RunRegasm(regasmPath, dllPath, true);

            // Update DLL
            IEdmFile5 file = AXC_Vault.Vault.Vault5.GetFileFromPath(dllPath, out IEdmFolder5 dllFolder);
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
            IEdmFile5 updaterFile = AXC_Vault.Vault.Vault5.GetFileFromPath(updaterPath, out IEdmFolder5 updaterFolder);
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

            using System; // Provides fundamental classes and base classes.
            using System.Collections.Generic; // Provides interfaces and classes that define generic collections.
            using System.Diagnostics; // Provides classes for working with processes, event logs, and performance counters.
            using System.Linq; // Provides classes and interfaces that support queries that use Language-Integrated Query (LINQ).
            using System.Runtime.InteropServices; // Provides a wide variety of members that support COM interop and platform invocation services.
            using System.Text; // Provides classes for manipulating strings.
            using System.Threading.Tasks; // Provides types that simplify the work of writing concurrent and asynchronous code.
            using SolidWorks.Interop.sldworks; // Provides SolidWorks interop classes for accessing SolidWorks API.
            using System.Windows.Forms; // Provides classes for creating Windows-based applications with rich UI.
            using System.Windows; // Provides types for WPF UI elements.
            using EPDM.Interop.epdm; // Provides interop classes for accessing Enterprise PDM (EPDM) API.
            using System.Threading; // Provides classes for working with threads.
            using static AddInUpdater.AddInUpdater; // Imports static members of the AddInUpdater class for easier access.
            using AXC_Vault; // References the AXC_Vault namespace.
            using static AXC_Vault.Vault; // Imports static members of the Vault class.

namespace AddInDllVersionControl
    {
        internal class AddInDllVersionControl
        {
            // Main method: the entry point of the program.
            static void Main()
            {
                // Paths to the necessary executable files and DLL.
                string regasmPath = @"C:\Windows\Microsoft.NET\Framework64\v4.0.30319\RegAsm.exe";
                string dllPath = @"C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting\Automation\Solidworks Add-In\Automation Guy.dll";
                string updaterPath = @"C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting\Automation\Add-In Updater\AddInUpdater.exe";

                // Close SolidWorks if it is running.
                foreach (Process process in Process.GetProcesses())
                {
                    if (process.ProcessName == "SLDWORKS")
                    {
                        try
                        {
                            // Get the active SolidWorks application object.
                            object swInstance = Marshal.GetActiveObject("SldWorks.Application");
                            SldWorks swApp = (SldWorks)swInstance;

                            // Show a warning message to the user.
                            MessageBox.Show("SolidWorks is about to close. " + "\n" +
                                            "Please ensure all work is saved before pressing OK to continue.",
                                            "Warning", MessageBoxButtons.OK, MessageBoxIcon.Warning);

                            // Close all open documents without saving.
                            swApp.CloseAllDocuments(false);

                            // Exit the SolidWorks application.
                            swApp.ExitApp();
                        }
                        catch (Exception ex)
                        {
                            Console.WriteLine("An exception occurred: " + ex.Message);
                        }
                    }
                }

                // Notify the user that the update process is starting.
                Console.WriteLine("Updating Automation Guy add-in...");

                // Unregister the existing DLL using RegAsm.
                RunRegasm(regasmPath, dllPath, true);

                // Update the DLL file from the vault.
                IEdmFile5 file = AXC_Vault.Vault.Vault5.GetFileFromPath(dllPath, out IEdmFolder5 dllFolder);
                file.GetFileCopy(0);
                if (file != null)
                {
                    Console.WriteLine($"Successfully updated {dllPath} to the current version");
                }
                else
                {
                    Console.WriteLine($"Could not update {dllPath} to the current version");
                }

                // Update the updater executable.
                IEdmFile5 updaterFile = AXC_Vault.Vault.Vault5.GetFileFromPath(updaterPath, out IEdmFolder5 updaterFolder);
                updaterFile.GetFileCopy(0);

                // Update all dependencies of the add-in DLL in the specified folder.
                GetAllFilesInFolder(dllFolder, false);

                // Register the updated DLL using RegAsm.
                RunRegasm(regasmPath, dllPath, false);

                // Attempt to relaunch SolidWorks.
                try
                {
                    // Path to the SolidWorks executable.
                    string solidWorksExePath = @"C:\Program Files\SOLIDWORKS Corp\SOLIDWORKS\SLDWORKS.exe";

                    // Start SolidWorks.
                    Process.Start(solidWorksExePath);
                }
                catch (Exception ex)
                {
                    Console.WriteLine("Error starting SolidWorks: " + ex.Message);
                }

                // Wait for user input before closing the console window.
                Console.WriteLine("\n\n\n" + "Press any key to close");
                Console.Read();
            }

            // Method to run RegAsm for registering or unregistering a DLL.
            private static void RunRegasm(string regasmPath, string dllPath, bool unregister)
            {
                var process = new Process
                {
                    StartInfo = new ProcessStartInfo
                    {
                        FileName = regasmPath, // Path to the RegAsm executable.
                        Arguments = $"/codebase \"{dllPath}\"", // Command-line arguments to run RegAsm.
                        Verb = "runas", // Run the process as an administrator.
                        RedirectStandardInput = true, // Redirect input/output streams.
                        RedirectStandardOutput = true,
                        RedirectStandardError = true,
                        UseShellExecute = false, // Do not use shell execution.
                        CreateNoWindow = true // Do not create a console window.
                    }
                };

                // Start the process and read its output.
                process.Start();
                var result = process.StandardError.ReadToEnd();
                process.WaitForExit();

                // Check the exit code to determine success or failure.
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

}
    }
}
