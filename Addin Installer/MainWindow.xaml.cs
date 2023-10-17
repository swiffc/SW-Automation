using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.IO;
using System.Windows;
using System.Linq;
using Microsoft.Win32;
using System.Diagnostics;
using System.Windows.Controls;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using SolidWorksAddinInstaller.Properties;

namespace SWinstaller
{
    public partial class MainWindow : INotifyPropertyChanged
    {
        private const string MRegAsmFilename = "RegAsm.exe";
        private const string MRegAsmWindowsPath = "Microsoft.NET\\Framework64";

        public ObservableCollection<string> InstalledAddInTitles
        {
            get => _installedAddInTitles;
            private set
            {
                _installedAddInTitles = value;
                OnPropertyChanged(); // Notify UI of changes
            }
        }
        private ObservableCollection<string> _installedAddInTitles = new ObservableCollection<string>();
        public ObservableCollection<string> PreviousAddInPaths { get; private set; } = new ObservableCollection<string>();

        public event PropertyChangedEventHandler PropertyChanged;

        // Constructor
        public MainWindow()
        {
            InitializeComponent();

            // Locate the RegAsm tool
            LocateRegAsm();

            // Set up data binding context
            DataContext = this;

            // Load previously used add-in paths
            ReadPreviousPaths();

            // Retrieve currently installed add-ins
            GetInstalledAddIns();
        }

        private void ReadPreviousPaths()
        {
            // Load paths from user settings
            var settings = Settings.Default.PreviousPaths;
            if (settings != null)
                PreviousAddInPaths = new ObservableCollection<string>(settings.Cast<string>());

            // Save paths when changes occur
            PreviousAddInPaths.CollectionChanged += SavePaths;
        }
        private void GetInstalledAddIns()
        {
            const RegistryHive hive = RegistryHive.LocalMachine;
            const RegistryView view = RegistryView.Registry64;
            const string keyPath = "SOFTWARE\\SolidWorks\\AddIns";

            var addInTitles = new List<string>();
            using (var registryKey = RegistryKey.OpenBaseKey(hive, view).OpenSubKey(keyPath))
            {
                if (registryKey == null)
                    return;

                // Iterate through subkeys to retrieve add-in titles
                foreach (var subKeyName in registryKey.GetSubKeyNames())
                {
                    using (var key = registryKey.OpenSubKey(subKeyName))
                    {
                        var value = (string)key?.GetValue("Title");
                        if (value == null || value.Equals("Presentation Manager"))
                            continue;

                        addInTitles.Add(value);
                    }
                }
            }

            InstalledAddInTitles = new ObservableCollection<string>(addInTitles);
        }
        private void OnPropertyChanged([CallerMemberName] string name = null) => PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(name));
        private void SavePaths(object sender, NotifyCollectionChangedEventArgs e)
        {
            var stringCollection = new StringCollection();
            stringCollection.AddRange(PreviousAddInPaths.ToArray());
            Settings.Default.PreviousPaths = stringCollection;
            Settings.Default.Save();
        }
        private void LocateRegAsm()
        {
            var results = new List<string>();
            FindByFilename(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Windows), MRegAsmWindowsPath), null, MRegAsmFilename, results);
            if (results?.Count > 0)
                RegAsmPath.Text = results.Last();
        }
        private static void FindByFilename(string path, string pathContains, string filename, List<string> results = null)
        {
            if (results == null)
                results = new List<string>();

            try
            {
                var files = Directory.EnumerateFiles(path).Where(f => string.Equals(Path.GetFileName(f), filename, StringComparison.InvariantCultureIgnoreCase)).ToList();
                if (files.Count > 0)
                    results.AddRange(files);
            }
            catch { }

            try
            {
                pathContains = pathContains?.ToLower();
                Directory.EnumerateDirectories(path).Where(f => string.IsNullOrEmpty(pathContains) || f.ToLower().Contains(pathContains)).ToList().ForEach(dir => FindByFilename(dir, null, filename, results));
            }
            catch { }
        }
        private void InstallAddin(string addinPath)
        {
            // Perform sanity checks
            if (!SanityCheck(RegAsmPath.Text, addinPath))
                return;

            // Create and configure a process to run RegAsm
            var process = new Process
            {
                StartInfo = new ProcessStartInfo
                {
                    FileName = RegAsmPath.Text,
                    Arguments = $"/codebase \"{addinPath}\"",
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
                AddPathToPreviousPaths(addinPath);
                GetInstalledAddIns();
                MessageBox.Show("Add-in was successfully registered", "Success");
            }
            else
                MessageBox.Show(result, "Unexpected Response", MessageBoxButton.OK, MessageBoxImage.Warning);
        }
        private static bool SanityCheck(string regasmPath, string dllPath)
        {
            // Check RegAsm
            if (string.IsNullOrEmpty(regasmPath))
            {
                MessageBox.Show("Please specify a path to a valid RegAsm application", "No RegAsm found");
                return false;
            }
            if (!File.Exists(regasmPath))
            {
                MessageBox.Show("The RegAsm file does not exist", "No RegAsm found");
                return false;
            }

            // Check Dll
            if (string.IsNullOrEmpty(dllPath))
            {
                MessageBox.Show("Please specify a path to a valid SolidWorks Add-in dll", "No Add-in found");
                return false;
            }
            if (!File.Exists(dllPath))
            {
                MessageBox.Show("The dll file does not exist", "File not found");
                return false;
            }

            return true;
        }
        private void UninstallAddin(string addinPath)
        {
            // Sanity check
            if (!SanityCheck(RegAsmPath.Text, addinPath))
                return;

            // Run the RegAsm with the Dll path as an argument
            var process = new Process
            {
                StartInfo = new ProcessStartInfo
                {
                    FileName = RegAsmPath.Text,
                    Arguments = $"/u \"{addinPath}\"",
                    // Run as admin
                    Verb = "runas",
                    // Redirect input and output
                    RedirectStandardInput = true,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    UseShellExecute = false,
                    CreateNoWindow = true
                }
            };

            process.Start();

            // Read the output
            var result = process.StandardError.ReadToEnd();
            process.WaitForExit();

            // If it exit successfully
            if (process.ExitCode == 0)
            {
                AddPathToPreviousPaths(addinPath);
                GetInstalledAddIns();
                MessageBox.Show("Add-in was successfully unregistered", "Success");
            }
            // Otherwise just show the results
            else
                MessageBox.Show(result, "Unexpected Response", MessageBoxButton.OK, MessageBoxImage.Warning);
        }
        private void AddPathToPreviousPaths(string addinPath)
        {
            if (!PreviousAddInPaths.Any(x => x.Equals(addinPath, StringComparison.InvariantCultureIgnoreCase)))
                PreviousAddInPaths.Add(addinPath);
        }
        private void BrowseRegAsmButton_Click(object sender, RoutedEventArgs e)
        {
            var ofd = new OpenFileDialog
            {
                Filter = $"RegAsm | {MRegAsmFilename}",
                InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.Windows)
            };

            // Open dialog
            var result = ofd.ShowDialog();

            // If they canceled, return
            if (!result.HasValue || !result.Value)
                return;

            // If they selected a file, use that
            RegAsmPath.Text = ofd.FileName;
        }
        private void BrowseDllButton_Click(object sender, RoutedEventArgs e)
        {
            var ofd = new OpenFileDialog
            {
                Filter = $"Add-in Dll | *.dll",
                InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)
            };

            // Open dialog
            var result = ofd.ShowDialog();

            // If they canceled, return
            if (!result.HasValue || !result.Value)
                return;

            // If they selected a file, use that
            DllPath.Text = ofd.FileName;
        }
        private void InstallButton_Click(object sender, RoutedEventArgs e) => InstallAddin(DllPath.Text);
        private void UninstallButton_Click(object sender, RoutedEventArgs e) => UninstallAddin(DllPath.Text);
        private void InstallPreviousAddIn_OnClick(object sender, RoutedEventArgs e)
        {
            // Make sure this gets called from a button
            if (!(sender is Button button))
                return;

            // Get the data context of the button, which should just a string path
            var addInPath = (string)button.DataContext;

            // Try to install the addin
            InstallAddin(addInPath);
        }
        private void UninstallPreviousAddIn_OnClick(object sender, RoutedEventArgs e)
        {
            // Make sure this gets called from a button
            if (!(sender is Button button))
                return;

            // Get the data context of the button, which should just a string path
            var addInPath = (string)button.DataContext;

            // Try to uninstall the addin
            UninstallAddin(addInPath);
        }
        private void RemovePathFromPrevious_OnClick(object sender, RoutedEventArgs e)
        {
            // Make sure this gets called from a button
            if (!(sender is Button button))
                return;

            // Get the data context of the button, which should just a string path
            var addInPath = (string)button.DataContext;

            // Remove the item from the list
            PreviousAddInPaths.Remove(addInPath);
        }
    }
}
