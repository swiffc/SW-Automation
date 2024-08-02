using EPDM.Interop.epdm;
using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swpublished;
using System;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
using System.Threading.Tasks;
using System.Windows.Forms;
using static FileTools.StaticFileTools;
using static FileTools.CommonData.CommonData;
using static Fork.ConfigFileManagement;


namespace SolidWorks_Add_In
{
    /// <summary>
    /// SolidWorks taskpane add-in
    /// </summary>
    public class TaskpaneIntegration : SwAddin
    {
        static public string VersionNumber => "7.0.2";
        static public string SpeechBubble =>
            @"""" + "Minor bugfix to BDL" + @"""";

        #region Private Members

        /// <summary>
        /// Cookie to the current instance of SolidWorks
        /// </summary>
        private int mSwCookie;

        /// <summary>
        /// Taskpane view
        /// </summary>
        private TaskpaneView mTaskpaneView;

        /// <summary>
        /// UI control that inside SolidWorks taskpane view
        /// </summary>
        private TaskpaneHostUI mTaskpaneHost;

        /// <summary>
        /// Current instance of SolidWorks
        /// </summary>
        private SldWorks mSolidWorksApplication;

        #endregion

        #region Public Members

        /// <summary>
        /// Unique taskpane ID used for COM registration
        /// </summary>
        public const string SWTASKPANE_PROGID = "Automation Guy Taskpane";

        #endregion

        #region SolidWorks Add-in Callbacks

        /// <summary>
        /// Called when add-in has loaded and wants to do connection logic
        /// </summary>
        /// <param name="ThisSW">The current SolidWorks instance</param>
        /// <param name="Cookie">The current SolidWorks cookie Id</param>
        /// <returns></returns>
        public bool ConnectToSW(object ThisSW, int Cookie)
        {
            // Store a reference to the current SolidWorks instance
            mSolidWorksApplication = (SldWorks)ThisSW;

            // Store cookie Id
            mSwCookie = Cookie;

            // Setup callback info
            var ok = mSolidWorksApplication.SetAddinCallbackInfo2(0, this, mSwCookie);

            // Version control
            if (!Developer)
            {
                VersionControl();
                SignInitials();
            }
            else
            {
                ClearConfigFileCache();
            }

            // Create our UI
            LoadUI();

            // Return ok
            return true;
        }
        public static void VersionControl()
        {
            string dLL = @"C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting\Automation\Solidworks Add-In\Automation Guy.dll";
            string updater = @"C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting\Automation\Add-In Updater\AddInUpdater.exe";
            string versionControl = @"C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting\Automation\Add-In Updater\AddInDllVersionControl.exe";

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

            // Update updater
            IEdmFile5 updaterFile = vault.GetFileFromPath(updater, out _);
            updaterFile.GetFileCopy(0);

            // Update version control
            IEdmFile5 versionControlFile = vault.GetFileFromPath(versionControl, out _);
            versionControlFile.GetFileCopy(0);

            // Check DLL version
            IEdmFile5 addinFile = vault.GetFileFromPath(dLL, out IEdmFolder5 dllFolder);
            IEdmFile12 addinFile12 = addinFile as IEdmFile12;

            bool localVersionObsolete;
            object filePath = addinFile.GetLocalPath(dllFolder.ID);
            int localVersionNo = addinFile12.GetLocalVersionNo2(ref filePath, out localVersionObsolete);
            int currentVersion = addinFile.CurrentVersion;

            // Check if the local version is older than the current version
            if (localVersionNo < currentVersion || localVersionObsolete)
            {
                ClearConfigFileCache();

                ProcessStartInfo startInfo = new ProcessStartInfo();
                startInfo.FileName = versionControl;
                Process.Start(startInfo);

                // Close Solidworks
                foreach (Process process in Process.GetProcesses())
                {
                    if (process.ProcessName == "SLDWORKS")
                    {
                        try
                        {
                            object swInstance = Marshal.GetActiveObject("SldWorks.Application");
                            SldWorks swApp = (SldWorks)swInstance;

                            // Close all documents without saving
                            swApp.CloseAllDocuments(false);

                            // Close SolidWorks
                            swApp.ExitApp();
                        }
                        catch (COMException)
                        {
                            // Handle the case where SolidWorks cannot be closed or is not responding
                        }
                    }
                }

            }
            else
            {
                ProcessStartInfo startInfo = new ProcessStartInfo();
                startInfo.FileName = updater;
                Process.Start(startInfo);
            }
        }


        /// <summary>
        /// Called when SolidWorks is about to unload add-in and wants to do disconnection logic
        /// </summary>
        /// <returns></returns>
        public bool DisconnectFromSW()
        {
            // Clean up our UI
            UnloadUI();

            // Return ok
            return true;
        }

        #endregion

        #region Create UI

        /// <summary>
        /// Create Taskpane and inject host UI
        /// </summary>
        private void LoadUI()
        {
            // Find location to our taskpane icon
            var imagePath = Path.Combine(Path.GetDirectoryName(typeof(TaskpaneIntegration).Assembly.CodeBase).Replace(@"file:\", string.Empty), "logo-small.bmp");

            // Create our Taskpane
            mTaskpaneView = mSolidWorksApplication.CreateTaskpaneView2(imagePath, $"Automation Guy {VersionNumber}");

            // Load our UI into the taskpane
            mTaskpaneHost = (TaskpaneHostUI)mTaskpaneView.AddControl(TaskpaneIntegration.SWTASKPANE_PROGID, string.Empty);
        }

        /// <summary>
        /// Cleanup taskpane during disconnect/unload
        /// </summary>
        private void UnloadUI()
        {
            mTaskpaneHost = null;

            // Remove taskpane view
            mTaskpaneView.DeleteView();

            // Release COM reference and cleanup memory
            Marshal.ReleaseComObject(mTaskpaneView);

            mTaskpaneView = null;
        }

        #endregion

        #region COM Registration

        /// <summary>
        /// COM registration call to add registry entries to SolidWorks add-in registry
        /// </summary>
        /// <param name="t"></param>
        [ComRegisterFunction()]
        private static void ComRegister(Type t)
        {
            var keyPath = string.Format(@"SOFTWARE\SolidWorks\AddIns\{0:b}", t.GUID);

            // Create our registry folder for the add-in
            using (var rk = Microsoft.Win32.Registry.LocalMachine.CreateSubKey(keyPath))
            {
                // Load add-in when SolidWorks opens
                rk.SetValue(null, 1);

                // Set SolidWorks add-in title and description
                rk.SetValue($"Title", "Automation Guy");
                rk.SetValue("Description", "Your new best friend");
            }
        }

        /// <summary>
        /// COM unregister call to remove custom entries added in COM register function
        /// </summary>
        /// <param name="t"></param>
        [ComUnregisterFunction()]
        private static void ComUnregister(Type t)
        {
            var keyPath = string.Format(@"SOFTWARE\SolidWorks\AddIns\{0:b}", t.GUID);

            // Remove our registry entry
            Microsoft.Win32.Registry.LocalMachine.DeleteSubKeyTree(keyPath);

        }

        #endregion
    }
}
