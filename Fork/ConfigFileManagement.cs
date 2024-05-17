using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Fork
{
    public class ConfigFileManagement
    {
        static public string ConfigPath
        {
            get
            {
                string localAppDataPath = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData);
                string chartPath = System.IO.Path.Combine(localAppDataPath, "Dassault_Systèmes_SolidWo");

                // Get the first subdirectory
                string[] subdirectories = Directory.GetDirectories(chartPath);
                if (subdirectories.Length == 0)
                {
                    // No subdirectories found
                    return null;
                }
                string firstSubdirectory = subdirectories[0];

                // Get the first subdirectory of the first subdirectory
                subdirectories = Directory.GetDirectories(firstSubdirectory);
                if (subdirectories.Length == 0)
                {
                    // No subdirectories found
                    return null;
                }
                string firstSubSubdirectory = subdirectories[0];

                // If file exists
                foreach (string filePath in Directory.EnumerateFiles(firstSubSubdirectory, "user.config", SearchOption.AllDirectories))
                    return filePath;

                // If file does not exist
                string whereTheConfigFileUsedToBe = Path.Combine(firstSubSubdirectory, "user.config");
                return whereTheConfigFileUsedToBe;
            }
        }

        [STAThread]
        static void Main()
        {
            ImportConfigFile();
        }

        public static void ExportConfigFile()
        {
            string configPath = ConfigPath;
            if (configPath == null)
            {
                MessageBox.Show("Config file not found.");
                return;
            }

            using (SaveFileDialog saveFileDialog = new SaveFileDialog())
            {
                saveFileDialog.Filter = "Config files (*.config)|*.config|All files (*.*)|*.*";
                saveFileDialog.FilterIndex = 1;
                saveFileDialog.RestoreDirectory = true;

                // Set the initial directory to the user's desktop
                saveFileDialog.InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.Desktop);

                if (saveFileDialog.ShowDialog() == DialogResult.OK)
                {
                    // Get the path of specified file
                    string filePath = saveFileDialog.FileName;

                    // Copy the config file to the selected location
                    File.Copy(configPath, filePath, true);
                }
            }
        }
        public static void ImportConfigFile()
        {
            string selectedFile = "ERROR";

            if (ConfigPath == null)
            {
                using (OpenFileDialog openFileDialog = new OpenFileDialog())
                {
                    openFileDialog.Filter = "Config files (*.config)|*.config|All files (*.*)|*.*";
                    openFileDialog.FilterIndex = 1;
                    openFileDialog.RestoreDirectory = true;

                    // Set the initial directory to the user's desktop
                    openFileDialog.InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.Desktop);

                    if (openFileDialog.ShowDialog() == DialogResult.OK)
                    {
                        // Get the path of specified file
                        selectedFile = openFileDialog.FileName;

                        // Create the path for the new config file
                        string newConfigPath = Path.Combine(ConfigPath, "user.config");

                        // Copy the selected file to the new config file location
                        File.Copy(selectedFile, newConfigPath, true);
                    }
                }
            }
            else
            {
                using (OpenFileDialog openFileDialog = new OpenFileDialog())
                {
                    openFileDialog.Filter = "Config files (*.config)|*.config|All files (*.*)|*.*";
                    openFileDialog.FilterIndex = 1;
                    openFileDialog.RestoreDirectory = true;

                    // Set the initial directory to the user's desktop
                    openFileDialog.InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.Desktop);

                    if (openFileDialog.ShowDialog() == DialogResult.OK)
                    {
                        // Get the path of specified file
                        selectedFile = openFileDialog.FileName;

                        // Overwrite the existing config file with the selected file
                        File.Copy(selectedFile, ConfigPath, true);
                    }
                }
            }
            MessageBox.Show($"{Path.GetFileNameWithoutExtension(selectedFile)} loaded", "Config file", MessageBoxButtons.OK, MessageBoxIcon.Information);
        }


    }
}
