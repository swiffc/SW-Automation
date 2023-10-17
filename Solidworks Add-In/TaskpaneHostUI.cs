using System;
using System.Drawing;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using Walkway;
using Walkway.Tools;
using static System.Windows.Forms.VisualStyles.VisualStyleElement;

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
            Drawing.Drawing.ActivatePreviousSheet();
        }

        private void NextSheet_Click(object sender, EventArgs e)
        {
            Drawing.Drawing.ActivateNextSheet();
        }

        private void PositionScale_Click(object sender, EventArgs e)
        {
            Drawing.Drawing.PositionAndScale();
        }

        private void AutoBalloon_Click(object sender, EventArgs e)
        {
            Drawing.Drawing.AutoBalloon();
        }

        private void AlignDimensions_Click(object sender, EventArgs e)
        {
            Drawing.Drawing.AlignDimensions();
        }

        private void DeleteDangling_Click(object sender, EventArgs e)
        {
            Drawing.Drawing.DeleteDanglingAnnotations();
        }

        private void SheetCleaner_Click(object sender, EventArgs e)
        {
            Drawing.Drawing.SheetCleaner();
        }

        private void SortSheets_Click(object sender, EventArgs e)
        {
            Drawing.Drawing.SortSheetsInDrawing();
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

        private void Version_MouseHover(object sender, EventArgs e)
        {
            Control control = sender as Control;
            if (control != null)
            {
                string tooltipText = "v1.0.0" + "\n" + @"""Everyone's gotta start somewhere!""";

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
            Drawing.Drawing.DrawingFileCleaner();
        }
    }
}
