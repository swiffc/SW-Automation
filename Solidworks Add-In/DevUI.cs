using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using static FileTools.Properties.Settings;
using static FileTools.CommonData.CommonData;
using static FileTools.StaticFileTools;
using static Tools.ModelTools;

namespace SolidWorks_Add_In
{
    public partial class DevUI : Form
    {
        public DevUI()
        {
            InitializeComponent();
        }

        private void button_ResetDefaults_Click(object sender, EventArgs e)
        {
            Default.Reset();
        }

        private void bExportTemplates_Click(object sender, EventArgs e)
        {
            SldWorks SW = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");
            string destinationFolderPath = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.Desktop), "Template Export");
            Directory.CreateDirectory(destinationFolderPath);

            foreach (Component2 component in (SW.IActiveDoc2 as AssemblyDoc).GetComponents(false))
            {
                string sourceFilePath = component.GetPathName();
                if (Path.GetExtension(sourceFilePath).ToLower() == ".sldprt")
                {
                    File.Copy(sourceFilePath, Path.Combine(destinationFolderPath, $"JOBNO-{component.ReferencedConfiguration}.sldprt"), true);
                }
            }
        }

        private void cDevMode_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cDevMode.Checked, x => DevMode = x);
        }

        private void DevUI_Load(object sender, EventArgs e)
        {
            cDevMode.Checked = DevMode;
        }

        private void button2_Click(object sender, EventArgs e)
        {
            SaveEverything();
        }

        private void button3_Click(object sender, EventArgs e)
        {
            SW.CloseAllDocuments(true);
        }
    }
}
