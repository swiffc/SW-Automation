using Hood;
using Plenum;
using Structure;
using System;
using System.Drawing;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using Walkway;
using Walkway.Tools;
using static System.Windows.Forms.VisualStyles.VisualStyleElement;
using static FileTools.Properties.Settings;
using MachineryMount;
using Bundle;
using HDR;

namespace SolidWorks_Add_In
{
    [ProgId(TaskpaneIntegration.SWTASKPANE_PROGID)]
    public partial class TaskpaneHostUI : UserControl
    {
        public TaskpaneHostUI()
        {
            InitializeComponent();
        }

        private void PreviousSheet_Click(object sender, EventArgs e)
        {
            DrawingToolz.DrawingToolz.ActivatePreviousSheet();
        }

        private void NextSheet_Click(object sender, EventArgs e)
        {
            DrawingToolz.DrawingToolz.ActivateNextSheet();
        }

        private void PositionScale_Click(object sender, EventArgs e)
        {
            DrawingToolz.DrawingToolz.PositionAndScale();
        }

        private void AutoBalloon_Click(object sender, EventArgs e)
        {
            DrawingToolz.DrawingToolz.AutoBalloon();
        }

        private void AlignDimensions_Click(object sender, EventArgs e)
        {
            DrawingToolz.DrawingToolz.AlignDimensions();
        }

        private void DeleteDangling_Click(object sender, EventArgs e)
        {
            DrawingToolz.DrawingToolz.DeleteDanglingAnnotations();
        }

        private void SheetCleaner_Click(object sender, EventArgs e)
        {
            DrawingToolz.DrawingToolz.SheetCleaner();
        }

        private void SortSheets_Click(object sender, EventArgs e)
        {
            DrawingToolz.DrawingToolz.SortSheetsInDrawing();
        }

        private void Unlock_Click(object sender, EventArgs e)
        {
            DevTools.Unlock();
        }

        private void CountHoles_Click(object sender, EventArgs e)
        {
            int holeCount = UserTools.CountHolesInAllSelectedFaces();
            MessageBox.Show($"Total holes in selected faces: {holeCount}", "Hole Count", MessageBoxButtons.OK, MessageBoxIcon.Information);
        }

        // Member variable to hold the form instance
        private WalkwayUI walkwayUI = null;
        private void WalkwayTool_Click(object sender, EventArgs e)
        {
            // Check if form is null or it has been disposed
            if (walkwayUI == null || walkwayUI.IsDisposed)
            {
                walkwayUI = new WalkwayUI();
            }

            walkwayUI.Show();
            walkwayUI.BringToFront();
        }

        private HoodUI hoodUI = null;
        private void btn_HoodUI_Click(object sender, EventArgs e)
        {
            if (hoodUI == null || hoodUI.IsDisposed)
            {
                hoodUI = new HoodUI();
            }

            hoodUI.Show();
            hoodUI.BringToFront();
        }




        private void Version_MouseHover(object sender, EventArgs e)
        {
            Control control = sender as Control;
            if (control != null)
            {

                string tooltipText = TaskpaneIntegration.VersionNumber + "\n" + TaskpaneIntegration.SpeechBubble;

                // Estimate the size of the tooltip text.
                SizeF textSize;
                using (Graphics graphics = control.CreateGraphics())
                {
                    textSize = graphics.MeasureString(tooltipText, control.Font); // Measure the size of the text.
                }

                int x = -control.Width - (int)textSize.Width; // Subtract the estimated width of the tooltip.
                int y = 0;

                toolTip1.AutoPopDelay = Int32.MaxValue;
                toolTip1.Show(tooltipText, control, x, y);
            }
        }


        private void Version_MouseLeave(object sender, EventArgs e)
        {
            toolTip1.Hide(sender as Control);
        }

        private void DrawingCleaner_Click(object sender, EventArgs e)
        {
            DrawingToolz.DrawingToolz.DrawingFileCleaner();
        }

        private void SplitDrawing_Click(object sender, EventArgs e)
        {
            DrawingToolz.DrawingFileManager.SplitActiveDrawing();
        }

        private void MigrateDrawing_Click(object sender, EventArgs e)
        {
            DrawingToolz.DrawingFileManager.AddToActiveDrawing();
        }

        private PlenumUI plenumUI = null;
        private void plenum_button_Click(object sender, EventArgs e)
        {
            if (plenumUI == null || plenumUI.IsDisposed)
            {
                plenumUI = new PlenumUI();
            }

            Default.Reload();
            plenumUI.Show();
            plenumUI.BringToFront();
        }

        private StructureUI structureUI = null;
        private void launch25_Click(object sender, EventArgs e)
        {
            if (structureUI == null || structureUI.IsDisposed)
            {
                structureUI = new StructureUI();
            }

            Default.Reload();
            structureUI.Show();
            structureUI.BringToFront();
        }

        private DevUI devUI = null;
        private void pictureBox1_Click(object sender, EventArgs e)
        {
            string desktopPath = Environment.GetFolderPath(Environment.SpecialFolder.Desktop).ToLower();
            if (desktopPath.Contains("acmurr"))
            {
                if (devUI == null || devUI.IsDisposed)
                    devUI = new DevUI();

                devUI.Show();
                devUI.BringToFront();
            }
        }

        private void button_ImportConfigFile_Click(object sender, EventArgs e)
        {
            Fork.ConfigFileManagement.ImportConfigFile();
        }

        private void button_ExportConfigFile_Click(object sender, EventArgs e)
        {
            Fork.ConfigFileManagement.ExportConfigFile();
        }

        private MachineryMountUI mmUI = null;
        private void launchMM_Click(object sender, EventArgs e)
        {
            if (mmUI == null || mmUI.IsDisposed)
            {
                mmUI = new MachineryMountUI();
            }

            Default.Reload();
            mmUI.Show();
            mmUI.BringToFront();
        }
        private BundleUI bundleUI = null;
        private void bBundle_Click(object sender, EventArgs e)
        {
            if (bundleUI == null || bundleUI.IsDisposed)
            {
                bundleUI = new BundleUI();
            }

            Default.Reload();
            bundleUI.Show();
            bundleUI.BringToFront();
        }

        private HeaderUI headerUI = null;
        private void header_Click(object sender, EventArgs e)
        {
            if (headerUI == null || headerUI.IsDisposed)
            {
                headerUI = new HeaderUI();
            }

            Default.Reload();
            headerUI.Show();
            headerUI.BringToFront();
        }
    }
}
