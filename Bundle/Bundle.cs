using Bundle.AirSeals.Derived;
using Bundle.TubeKeepers;
using FileTools.Base;
using System;
using System.Windows.Forms;
using static FileTools.CommonData.CommonData;
using Bundle.Misc;
using Bundle.TubeSupports;
using Bundle.SideFrame.Derived.Children;
using Bundle.SideFrame;
using Bundle.SideFrame.Derived;
using Bundle.SideFrame.Derived.Children.Derived;
using FileTools.Infrastructure;
using static FileTools.StaticFileTools;

namespace Bundle
{
    internal class Bundle : MainAssembly
    {
        // Static properties
        static public double Width
        {
            get { return Bundle_Width; }
            set { Bundle_Width = value; }
        }
        static public IHeaderExtensions LowestFrontHeader
        {
            get
            {
                IHeaderExtensions lowestHeader;
                if (Header65.IsRequired)
                    lowestHeader = Header65;
                else if (Header63.IsRequired)
                    lowestHeader = Header63;
                else lowestHeader = Header61;
                return lowestHeader;
            }
        }
        static public IHeaderExtensions LowestRearHeader
        {
            get
            {
                IHeaderExtensions lowestHeader;
                if (Header66.IsRequired)
                    lowestHeader = Header66;
                else if (Header64.IsRequired)
                    lowestHeader = Header64;
                else lowestHeader = Header62;
                return lowestHeader;
            }
        }


        // Constructor
        public Bundle(int assemblyNumber, string assemblyDescription) : base(assemblyNumber, assemblyDescription
            //, typeof(Top_Front_AirSeal)
            )
        { }


        // Method overrides
        protected override void Dimensions()
        {
            if (ClassesToIsolate.Count == 0)
            {
                if (Header61.IsRequired)
                {
                    EditDimension("Length", "61", Header61.BoxLength);
                    EditDimension("Coverspan", "61", Header61.BoxWidth + Header61.TubesheetTHK + Header61.PlugsheetTHK);
                    EditDimension("Height", "61", Header61.BoxHeight + Header61.TopAndBottomPlateTHK * 2);
                    EditDimension("Y", "61", Header61.TubeY - Header61.TopAndBottomPlateTHK - Header61.BoxHeight);
                    EditDimension("Z", "61", TubeLength / 2 - TubeProjection - Header61.TubesheetTHK);
                }
                if (Header62.IsRequired)
                {
                    EditDimension("Length", "62", Header62.BoxLength);
                    EditDimension("Coverspan", "62", Header62.BoxWidth + Header62.TubesheetTHK + Header62.PlugsheetTHK);
                    EditDimension("Height", "62", Header62.BoxHeight + Header62.TopAndBottomPlateTHK * 2);
                    EditDimension("Y", "62", Header62.TubeY - Header62.TopAndBottomPlateTHK - Header62.BoxHeight);
                    EditDimension("Z", "62", TubeLength / 2 - TubeProjection - Header62.TubesheetTHK);
                }
                if (Header63.IsRequired)
                {
                    EditDimension("Length", "63", Header63.BoxLength);
                    EditDimension("Coverspan", "63", Header63.BoxWidth + Header63.TubesheetTHK + Header63.PlugsheetTHK);
                    EditDimension("Height", "63", Header63.BoxHeight + Header63.TopAndBottomPlateTHK * 2);
                    EditDimension("Y", "63", Header63.TubeY - Header63.TopAndBottomPlateTHK - Header63.BoxHeight);
                    EditDimension("Z", "63", TubeLength / 2 - TubeProjection - Header63.TubesheetTHK);
                }
                if (Header64.IsRequired)
                {
                    EditDimension("Length", "64", Header64.BoxLength);
                    EditDimension("Coverspan", "64", Header64.BoxWidth + Header64.TubesheetTHK + Header64.PlugsheetTHK);
                    EditDimension("Height", "64", Header64.BoxHeight + Header64.TopAndBottomPlateTHK * 2);
                    EditDimension("Y", "64", Header64.TubeY - Header64.TopAndBottomPlateTHK - Header64.BoxHeight);
                    EditDimension("Z", "64", TubeLength / 2 - TubeProjection - Header64.TubesheetTHK);
                }
                if (Header65.IsRequired)
                {
                    EditDimension("Length", "65", Header65.BoxLength);
                    EditDimension("Coverspan", "65", Header65.BoxWidth + Header65.TubesheetTHK + Header65.PlugsheetTHK);
                    EditDimension("Height", "65", Header65.BoxHeight + Header65.TopAndBottomPlateTHK * 2);
                    EditDimension("Y", "65", Header65.TubeY - Header65.TopAndBottomPlateTHK - Header65.BoxHeight);
                    EditDimension("Z", "65", TubeLength / 2 - TubeProjection - Header65.TubesheetTHK);
                }
                if (Header66.IsRequired)
                {
                    EditDimension("Length", "66", Header66.BoxLength);
                    EditDimension("Coverspan", "66", Header66.BoxWidth + Header66.TubesheetTHK + Header66.PlugsheetTHK);
                    EditDimension("Height", "66", Header66.BoxHeight + Header66.TopAndBottomPlateTHK * 2);
                    EditDimension("Y", "66", Header66.TubeY - Header66.TopAndBottomPlateTHK - Header66.BoxHeight);
                    EditDimension("Z", "66", TubeLength / 2 - TubeProjection - Header66.TubesheetTHK);
                }
            }
        }
        protected override void Sketches()
        {
            if (ClassesToIsolate.Count == 0)
            {
                if (Header61.IsRequired)
                    UnsuppressSketch("61");
                else SuppressSketch("61");

                if (Header62.IsRequired)
                    UnsuppressSketch("62");
                else SuppressSketch("62");

                if (Header63.IsRequired)
                    UnsuppressSketch("63");
                else SuppressSketch("63");

                if (Header64.IsRequired)
                    UnsuppressSketch("64");
                else SuppressSketch("64");

                if (Header65.IsRequired)
                    UnsuppressSketch("65");
                else SuppressSketch("65");

                if (Header66.IsRequired)
                    UnsuppressSketch("66");
                else SuppressSketch("66");
            }
        }


        // Debugging
        [STAThread]
        static void Main()
        {
            // STEP 1: Initialize global error handler FIRST - before anything else
            GlobalErrorHandler.Initialize();

            try
            {
                GlobalErrorHandler.LogInfo("=== BundleApp Starting ===");
                GlobalErrorHandler.LogInfo($"OS: {Environment.OSVersion}");
                GlobalErrorHandler.LogInfo($".NET Version: {Environment.Version}");

                // STEP 2: Configure Windows Forms
                Application.EnableVisualStyles();
                Application.SetCompatibleTextRenderingDefault(false);

                // STEP 3: Start the application
                GlobalErrorHandler.LogInfo("Launching BundleUI");
                Application.Run(new BundleUI());

                GlobalErrorHandler.LogInfo("=== Application closed normally ===");
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Main Entry Point - Fatal Error");
                MessageBox.Show(
        $"A fatal error occurred during startup:\n\n{ex.Message}\n\n" +
      $"The application will now close.\n\n" +
        $"Please check the log file for details:\n{GlobalErrorHandler.LogFilePath}",
               "Fatal Startup Error",
              MessageBoxButtons.OK,
         MessageBoxIcon.Error);
            }
            finally
            {
                // STEP 4: Cleanup
                try
            {
                    DisconnectSolidWorks();
                 GlobalErrorHandler.LogInfo("Cleanup completed");
              }
                catch (Exception ex)
            {
                    GlobalErrorHandler.LogWarning($"Error during cleanup: {ex.Message}");
                 }
            }
         }
    }
}
