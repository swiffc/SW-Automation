using EPDM.Interop.epdm;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Net.NetworkInformation;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Windows.Input;
using static Plenum.Plenum;
using mTools = Tools.ModelTools;
using cTools = ModelTools.ReleaseCOM;
using Plenum.Floor;
using Plenum.Helpers.Static;
using ModelTools;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;
using static FileTools.Properties.Settings;

namespace Plenum.StandardParts
{
    internal class FanGuard : IComponentInfo
    {
        public static bool Enabled
        {
            get
            {
                string guardFolderPath = @"C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\templates\common_library\Fan Guards";
                StaticFanGuardDictionary.TryGetValue(FanDiameter_Inches, out string partNo);
                string guardFileName = partNo + ".SLDPRT";
                string guardFilePath = Path.Combine(guardFolderPath, guardFileName);

                bool FileExists = File.Exists(guardFilePath);
                return FileExists ? true : false;
            }
        }

        // Standard part numbers
        internal static Dictionary<double, string> StaticFanGuardDictionary = new Dictionary<double, string>
        {
            { 36, "FG36" },
            { 42, "FG42" },
            { 48, "FG48" },
            { 54, "FG54" },
            { 60, "FG60" },
            { 66, "FG66" },
            { 72, "FG72" },
            { 78, "FG78" },
            { 84, "FG84" },
            { 90, "FG90" },
            { 96, "FG96" },
            { 102, "FG102" },
            { 108, "FG108" },
            { 114, "FG114" },
            { 120, "FG120" },
            { 126, "FG126" },
            { 132, "FG132" },
            { 138, "FG138" },
            { 144, "FG144" },
            { 150, "FG150" },
            { 156, "FG156" },
            { 162, "FG162" },
            { 168, "FG168" },
            { 180, "FG180" }
        };


        // Properties
        private Design CallerType { get; set; }
        public string PartNo
        {
            get
            {
                StaticFanGuardDictionary.TryGetValue(FanDiameter_Inches, out string partNo);
                return partNo;
            }
        }
        public string FilePath
        {
            get
            {
                string guardFolderPath = @"C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\templates\common_library\Fan Guards";
                string guardFileName = PartNo + ".SLDPRT";
                string guardFilePath = Path.Combine(guardFolderPath, guardFileName);

                bool FileExists = File.Exists(guardFilePath);
                if (FileExists)
                    return guardFilePath;

                // Log into vault
                EdmVault5 vault = new EdmVault5();
                vault.LoginAuto("AXC_VAULT", 0);

                // Get file
                IEdmFolder5 folder = vault.GetFolderFromPath(Path.GetDirectoryName(guardFilePath));
                try
                {
                    IEdmFile5 file = folder.GetFile(Path.GetFileName(guardFilePath));
                    file.GetFileCopy(0);
                    cTools.Release(ref file);
                }
                catch (Exception)
                {
                    MessageBox.Show($"Could not find fan guard {guardFilePath}");
                }

                // Load into memory
                mTools.DisablePartUI();
                mTools.Open(guardFilePath);
                mTools.EnablePartUI();

                // Release resources
                cTools.Release(ref folder);
                cTools.Release(ref vault);

                if (FileExists)
                    return guardFilePath;

                return null;
            }
        }
        protected List<PositionData> _position;
        public List<PositionData> Position
        {
            get
            {
                _position = new List<PositionData>();

                if (MotorShaftDown)
                {
                    double yTranslation = Plenum_Depth - Math.Max(SidePanel_THK, EndPanel_THK) + Default.FanRing_Depth;
                    var zTranslation = FanCenter.ZTranslation(CallerType);

                    for (int i = 0; i < Fan_Count; i++)
                    {
                        double check = zTranslation[i];
                        _position.Add(PositionData.Create(tY: -yTranslation, tZ: zTranslation[i]));
                    }
                }

                return _position;
            }
        }
        public string StaticPartNo
        {
            get
            {
                StaticFanGuardDictionary.TryGetValue(FanDiameter_Inches, out string partNo);
                return partNo;
            }
        }


        // Constructor
        public FanGuard(Design callerType)
        {
            CallerType = callerType;

            string guardFolderPath = @"C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\templates\common_library\Fan Guards";
            string guardFileName = PartNo + ".SLDPRT";
            string guardFilePath = Path.Combine(guardFolderPath, guardFileName);

            bool FileExists = File.Exists(guardFilePath);
            if (FileExists)
            {
                ModelDoc2 modelDoc2 = mTools.Open(FilePath, StaticPartNo);

                cTools.Release(ref modelDoc2);
            }

        }
    }
}
