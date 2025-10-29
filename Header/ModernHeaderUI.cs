using System;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using static Excel.StaticHelpers;
using static FileTools.CommonData.CommonData;
using static HDR.HeaderBase;

namespace HDR
{
    public partial class ModernHeaderUI : Form
    {
        // For draggable window
        private bool dragging = false;
        private Point dragCursorPoint;
        private Point dragFormPoint;

        // Animation
        private Timer hoverTimer;
        private Control hoveredButton;
        private float buttonHoverProgress = 0f;

        public ModernHeaderUI()
        {
            InitializeComponent();
            
            // Enable double buffering for smooth animations
            this.DoubleBuffered = true;
            this.SetStyle(ControlStyles.ResizeRedraw, true);
            this.SetStyle(ControlStyles.OptimizedDoubleBuffer, true);
            this.SetStyle(ControlStyles.AllPaintingInWmPaint, true);
            
            // Add shadow effect
            AddDropShadow();
            
            // Setup hover animation timer
            hoverTimer = new Timer();
            hoverTimer.Interval = 16; // ~60 FPS
            hoverTimer.Tick += HoverTimer_Tick;
            
            // Add hover effects to buttons
            foreach (Control control in footerPanel.Controls)
            {
                if (control is Button button)
                {
                    button.MouseEnter += Button_MouseEnter;
                    button.MouseLeave += Button_MouseLeave;
                }
            }
            
            // Setup header data event handlers (from original HeaderUI)
            Header_TextChanged(this, "Box");
            Header_TextChanged(this, "Tubesheet");
            Header_TextChanged(this, "Plugsheet");
            Header_TextChanged(this, "TopBtm");
            Header_TextChanged(this, "EndPlate");
            Header_TextChanged(this, "Tube");
            Header_TextChanged(this, "Stiffener");
            Header_TextChanged(this, "Partition");
            Header_TextChanged(this, "Foot");
            
            Connection_TextChanged(this, "Inlet");
            Connection_TextChanged(this, "Outlet");
        }

        #region Shadow Effect
        
        [DllImport("Gdi32.dll", EntryPoint = "CreateRoundRectRgn")]
        private static extern IntPtr CreateRoundRectRgn(
            int nLeftRect, int nTopRect, int nRightRect, int nBottomRect,
            int nWidthEllipse, int nHeightEllipse);

        [DllImport("dwmapi.dll")]
        private static extern int DwmExtendFrameIntoClientArea(IntPtr hWnd, ref MARGINS pMarInset);

        [DllImport("dwmapi.dll")]
        private static extern int DwmSetWindowAttribute(IntPtr hwnd, int attr, ref int attrValue, int attrSize);

        [DllImport("dwmapi.dll")]
        private static extern int DwmIsCompositionEnabled(ref int pfEnabled);

        private struct MARGINS
        {
            public int leftWidth;
            public int rightWidth;
            public int topHeight;
            public int bottomHeight;
        }

        private const int DWMWA_USE_IMMERSIVE_DARK_MODE = 20;
        private const int DWMWA_WINDOW_CORNER_PREFERENCE = 33;
        private const int DWMWCP_ROUND = 2;

        private void AddDropShadow()
        {
            int value = 1;
            DwmSetWindowAttribute(this.Handle, DWMWA_USE_IMMERSIVE_DARK_MODE, ref value, sizeof(int));
            
            value = DWMWCP_ROUND;
            DwmSetWindowAttribute(this.Handle, DWMWA_WINDOW_CORNER_PREFERENCE, ref value, sizeof(int));
        }
        
        #endregion

        #region Window Dragging
        
        private void HeaderPanel_MouseDown(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left)
            {
                dragging = true;
                dragCursorPoint = Cursor.Position;
                dragFormPoint = this.Location;
            }
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            if (dragging)
            {
                Point dif = Point.Subtract(Cursor.Position, new Size(dragCursorPoint));
                this.Location = Point.Add(dragFormPoint, new Size(dif));
            }
            base.OnMouseMove(e);
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            dragging = false;
            base.OnMouseUp(e);
        }
        
        #endregion

        #region Custom Painting

        private void Form_Paint(object sender, PaintEventArgs e)
        {
            // Draw outer border
            using (Pen pen = new Pen(Color.FromArgb(26, 32, 44), 1))
            {
                e.Graphics.DrawRectangle(pen, 0, 0, this.Width - 1, this.Height - 1);
            }
        }

        private void HeaderPanel_Paint(object sender, PaintEventArgs e)
        {
            // Create gradient background
            using (LinearGradientBrush brush = new LinearGradientBrush(
                headerPanel.ClientRectangle,
                Color.FromArgb(26, 32, 44),
                Color.FromArgb(45, 55, 72),
                LinearGradientMode.Horizontal))
            {
                e.Graphics.FillRectangle(brush, headerPanel.ClientRectangle);
            }

            // Add subtle accent line at bottom
            using (Pen pen = new Pen(Color.FromArgb(124, 58, 237), 2))
            {
                e.Graphics.DrawLine(pen, 0, headerPanel.Height - 2, headerPanel.Width, headerPanel.Height - 2);
            }
        }

        private void Footer_Paint(object sender, PaintEventArgs e)
        {
            // Draw top border
            using (Pen pen = new Pen(Color.FromArgb(226, 232, 240), 1))
            {
                e.Graphics.DrawLine(pen, 0, 0, footerPanel.Width, 0);
            }
        }

        private void Panel_Paint(object sender, PaintEventArgs e)
        {
            Panel panel = sender as Panel;
            if (panel == null) return;

            e.Graphics.SmoothingMode = SmoothingMode.AntiAlias;

            // Draw rounded rectangle background
            using (GraphicsPath path = GetRoundedRectangle(panel.ClientRectangle, 8))
            {
                e.Graphics.FillPath(new SolidBrush(panel.BackColor), path);

                // Draw subtle border
                using (Pen pen = new Pen(Color.FromArgb(226, 232, 240), 1))
                {
                    e.Graphics.DrawPath(pen, path);
                }
            }
        }

        private void Button_Paint(object sender, PaintEventArgs e)
        {
            Button button = sender as Button;
            if (button == null) return;

            e.Graphics.SmoothingMode = SmoothingMode.AntiAlias;

            // Create rounded rectangle
            using (GraphicsPath path = GetRoundedRectangle(new Rectangle(0, 0, button.Width, button.Height), 6))
            {
                // Fill with button color (with hover effect)
                Color buttonColor = button.BackColor;
                if (hoveredButton == button && buttonHoverProgress > 0)
                {
                    buttonColor = LightenColor(buttonColor, buttonHoverProgress * 0.15f);
                }

                using (SolidBrush brush = new SolidBrush(buttonColor))
                {
                    e.Graphics.FillPath(brush, path);
                }

                // Draw text
                TextRenderer.DrawText(e.Graphics, button.Text, button.Font, button.ClientRectangle,
                    button.ForeColor, TextFormatFlags.HorizontalCenter | TextFormatFlags.VerticalCenter);
            }
        }

        private void TextBox_Paint(object sender, PaintEventArgs e)
        {
            // Custom textbox border will be drawn by parent panel
        }

        private void TabControl_DrawItem(object sender, DrawItemEventArgs e)
        {
            TabControl tabControl = sender as TabControl;
            Graphics g = e.Graphics;
            g.SmoothingMode = SmoothingMode.AntiAlias;

            Rectangle tabRect = tabControl.GetTabRect(e.Index);
            bool isSelected = (e.Index == tabControl.SelectedIndex);

            // Background
            Color backColor = isSelected ? Color.White : Color.FromArgb(248, 250, 252);
            using (SolidBrush brush = new SolidBrush(backColor))
            {
                g.FillRectangle(brush, tabRect);
            }

            // Accent bar for selected tab
            if (isSelected)
            {
                using (SolidBrush brush = new SolidBrush(Color.FromArgb(124, 58, 237)))
                {
                    Rectangle accentRect = new Rectangle(tabRect.X, tabRect.Bottom - 3, tabRect.Width, 3);
                    g.FillRectangle(brush, accentRect);
                }
            }

            // Text
            Color textColor = isSelected ? Color.FromArgb(15, 23, 42) : Color.FromArgb(100, 116, 139);
            using (SolidBrush brush = new SolidBrush(textColor))
            {
                StringFormat sf = new StringFormat
                {
                    Alignment = StringAlignment.Center,
                    LineAlignment = StringAlignment.Center
                };
                g.DrawString(tabControl.TabPages[e.Index].Text, tabControl.Font, brush, tabRect, sf);
            }
        }

        #endregion

        #region Hover Animation

        private void Button_MouseEnter(object sender, EventArgs e)
        {
            hoveredButton = sender as Control;
            buttonHoverProgress = 0f;
            hoverTimer.Start();
        }

        private void Button_MouseLeave(object sender, EventArgs e)
        {
            hoveredButton = null;
        }

        private void HoverTimer_Tick(object sender, EventArgs e)
        {
            if (hoveredButton != null)
            {
                buttonHoverProgress += 0.1f;
                if (buttonHoverProgress >= 1f)
                {
                    buttonHoverProgress = 1f;
                    hoverTimer.Stop();
                }
                hoveredButton.Invalidate();
            }
            else
            {
                buttonHoverProgress -= 0.15f;
                if (buttonHoverProgress <= 0f)
                {
                    buttonHoverProgress = 0f;
                    hoverTimer.Stop();
                }
            }
        }

        #endregion

        #region TextBox Focus Effects

        private void TextBox_Enter(object sender, EventArgs e)
        {
            TextBox textBox = sender as TextBox;
            if (textBox != null)
            {
                // Add glow effect on focus
                textBox.BackColor = Color.FromArgb(239, 246, 255);
            }
        }

        private void TextBox_Leave(object sender, EventArgs e)
        {
            TextBox textBox = sender as TextBox;
            if (textBox != null)
            {
                textBox.BackColor = Color.White;
            }
        }

        #endregion

        #region Helper Methods

        private GraphicsPath GetRoundedRectangle(Rectangle bounds, int radius)
        {
            int diameter = radius * 2;
            Size size = new Size(diameter, diameter);
            Rectangle arc = new Rectangle(bounds.Location, size);
            GraphicsPath path = new GraphicsPath();

            if (radius == 0)
            {
                path.AddRectangle(bounds);
                return path;
            }

            // Top left arc
            path.AddArc(arc, 180, 90);

            // Top right arc
            arc.X = bounds.Right - diameter;
            path.AddArc(arc, 270, 90);

            // Bottom right arc
            arc.Y = bounds.Bottom - diameter;
            path.AddArc(arc, 0, 90);

            // Bottom left arc
            arc.X = bounds.Left;
            path.AddArc(arc, 90, 90);

            path.CloseFigure();
            return path;
        }

        private Color LightenColor(Color color, float percent)
        {
            int r = Math.Min(255, (int)(color.R + (255 - color.R) * percent));
            int g = Math.Min(255, (int)(color.G + (255 - color.G) * percent));
            int b = Math.Min(255, (int)(color.B + (255 - color.B) * percent));
            return Color.FromArgb(color.A, r, g, b);
        }

        #endregion

        #region Event Handlers

        private void ModernHeaderUI_Load(object sender, EventArgs e)
        {
            statusStrip.Text = "? Ready - Standalone Mode";
        }

        private void CloseButton_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        private void MinimizeButton_Click(object sender, EventArgs e)
        {
            this.WindowState = FormWindowState.Minimized;
        }

        private void ImportPregoButton_Click(object sender, EventArgs e)
        {
            statusStrip.Text = "?? Importing Prego data...";
            // TODO: Connect to existing Prego import functionality
            MessageBox.Show("Import Prego functionality will be connected to existing code.",
                "Import Prego", MessageBoxButtons.OK, MessageBoxIcon.Information);
            statusStrip.Text = "? Ready";
        }

        private void CreateButton_Click(object sender, EventArgs e)
        {
            statusStrip.Text = "?? Creating/Updating header...";
            // TODO: Connect to existing create/update functionality
            MessageBox.Show("Create/Update functionality will be connected to existing code.",
                "Create/Update", MessageBoxButtons.OK, MessageBoxIcon.Information);
            statusStrip.Text = "? Ready";
        }

        private void RunButton_Click(object sender, EventArgs e)
        {
            statusStrip.Text = "?? Running automation...";
            // TODO: Connect to existing run functionality
            MessageBox.Show("Run functionality will be connected to existing code.",
                "Run", MessageBoxButtons.OK, MessageBoxIcon.Information);
            statusStrip.Text = "? Complete";
        }

        #endregion
    }
}
