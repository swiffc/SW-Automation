using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Linq;
using System.Windows.Forms;

namespace HDR
{
    /// <summary>
    /// Modern tabbed Header tool form inspired by contemporary UI design
    /// </summary>
    public partial class ModernHeaderForm : Form
    {
        // UI State
        private Dictionary<int, bool> _activeConnections = new Dictionary<int, bool>
        {
            { 66, false }, { 65, false }, { 64, false },
            { 63, false }, { 62, true }, { 61, true }
        };

        private string _inputMode = "traditional"; // "traditional" or "single"
        private int _selectedConnectionIndex = 0;
        private readonly int[] _connectionNumbers = { 66, 65, 64, 63, 62, 61 };
        private readonly string[] _connectionNames = { "Temp", "Press", "Drain", "Vent", "Outlet", "Inlet" };
        private readonly Color[] _connectionColors = 
        {
            Color.FromArgb(59, 130, 246),   // Blue
            Color.FromArgb(16, 185, 129),   // Green
            Color.FromArgb(124, 58, 237),   // Purple
            Color.FromArgb(249, 115, 22),   // Orange
            Color.FromArgb(236, 72, 153),   // Pink
            Color.FromArgb(6, 182, 212)     // Cyan
        };

        // Form Data Storage
        private Dictionary<string, string[]> _formData;

        public ModernHeaderForm()
        {
            InitializeFormData();
            InitializeComponent();
            SetupModernStyles();
        }

        private void InitializeFormData()
        {
            _formData = new Dictionary<string, string[]>();
            
            // Initialize all form fields with 6 empty strings (one per connection)
            var fields = new[]
            {
                // Box dimensions
                "boxWidth", "boxHeight",
                
                // Thickness
                "thkTubesheet", "thkPlugsheet", "thkTopBottomPlate", "thkEndPlate",
                
                // Width
                "widthTubesheet", "widthPlugsheet", "widthTopBottomPlate", "widthEndPlate",
                
                // Length
                "lengthTubesheet", "lengthPlugsheet", "lengthTopBottomPlate", 
                "lengthEndPlateBusted2",
                
                // Diameter
                "diameterTubeHoles",
                
                // Offset
                "yOffsetTubeRow1", "xOffsetTubeRowsOdd", "xOffsetTubeRowsEven",
                
                // Count (12 rows)
                "countRow1", "countRow2", "countRow3", "countRow4", "countRow5", "countRow6",
                "countRow7", "countRow8", "countRow9", "countRow10", "countRow11", "countRow12",
                
                // Pitch
                "hPitchRowOdd", "hPitchRowEven",
                "vPitchRows1And2", "vPitchRows2And3", "vPitchRows3And4", "vPitchRows4And5",
                "vPitchRows5And6", "vPitchRows6And7", "vPitchRows7And8", "vPitchRows8And9",
                "vPitchRows9And10", "vPitchRows10And11", "vPitchRows11And12",
                
                // Stiffener
                "thkStiffener", "widthStiffener", "offsetStiffener",
                
                // Window (Stiffener)
                "windowWidthStiffener", "windowLengthStiffener", "windowQuantityStiffener",
                "windowSpacingStiffener", "windowOffsetStiffener",
                
                // Location (Stiffener)
                "locationBelowTubeRow", "distanceBelowRow",
                "location2BelowTubeRow", "distance2BelowRow",
                "location3BelowTubeRow", "distance3BelowRow",
                
                // Partition
                "thkPartition", "widthPartition",
                "locationBelowTubeRowPartition", "distanceBelowRowPartition",
                "location2BelowTubeRowPartition",
                
                // Connection-specific fields
                "count", "spacing", "offset", "projection", "extension",
                "o", "q", "r", "x", "rd", "nb", "db", "bc", "yy", "od", "wall", "partno"
            };

            foreach (var field in fields)
            {
                _formData[field] = new string[6] { "", "", "", "", "", "" };
            }
            
            // Boolean checkbox fields
            _formData["lengthEndPlateBusted"] = new string[6] { "false", "false", "false", "false", "false", "false" };
        }

        private void SetupModernStyles()
        {
            // Form settings
            this.BackColor = ColorTranslator.FromHtml("#F8FAFC");
            this.Font = new Font("Segoe UI", 9.75F);
            this.DoubleBuffered = true;
            
            // Enable modern visual styles
            Application.EnableVisualStyles();
        }

        // Helper method to create modern button
        private Button CreateModernButton(string text, Color baseColor, EventHandler clickHandler = null)
        {
            var button = new Button
            {
                Text = text,
                Font = new Font("Segoe UI", 10F, FontStyle.Bold),
                ForeColor = Color.White,
                BackColor = baseColor,
                FlatStyle = FlatStyle.Flat,
                Cursor = Cursors.Hand,
                Height = 45
            };

            button.FlatAppearance.BorderSize = 0;
            button.FlatAppearance.MouseOverBackColor = ControlPaint.Light(baseColor, 0.1f);
            button.FlatAppearance.MouseDownBackColor = ControlPaint.Dark(baseColor, 0.1f);

            if (clickHandler != null)
                button.Click += clickHandler;

            return button;
        }

        // Helper method to create modern textbox
        private TextBox CreateModernTextBox(string fieldName, int connectionIndex, bool enabled = true)
        {
            var textBox = new TextBox
            {
                Font = new Font("Segoe UI", 9.75F),
                BorderStyle = BorderStyle.FixedSingle,
                Height = 32,
                Enabled = enabled,
                BackColor = enabled ? Color.White : ColorTranslator.FromHtml("#F1F5F9"),
                ForeColor = enabled ? Color.Black : ColorTranslator.FromHtml("#94A3B8"),
                Tag = new Tuple<string, int>(fieldName, connectionIndex)
            };

            // Bind to form data
            if (_formData.ContainsKey(fieldName))
            {
                textBox.Text = _formData[fieldName][connectionIndex];
                textBox.TextChanged += (s, e) =>
                {
                    var tb = s as TextBox;
                    var tag = tb.Tag as Tuple<string, int>;
                    if (tag != null && _formData.ContainsKey(tag.Item1))
                    {
                        _formData[tag.Item1][tag.Item2] = tb.Text;
                    }
                };
            }

            return textBox;
        }

        // Helper method to create modern combobox
        private ComboBox CreateModernComboBox(bool enabled = true)
        {
            var comboBox = new ComboBox
            {
                Font = new Font("Segoe UI", 9.75F),
                FlatStyle = FlatStyle.Flat,
                Height = 32,
                Enabled = enabled,
                BackColor = enabled ? Color.White : ColorTranslator.FromHtml("#F1F5F9"),
                DropDownStyle = ComboBoxStyle.DropDownList
            };

            comboBox.Items.Add("-");
            comboBox.SelectedIndex = 0;

            return comboBox;
        }

        // Helper method to create section header
        private Label CreateSectionHeader(string text, Color accentColor)
        {
            var label = new Label
            {
                Text = text,
                Font = new Font("Segoe UI", 11F, FontStyle.Bold),
                ForeColor = ColorTranslator.FromHtml("#334155"),
                AutoSize = true,
                Padding = new Padding(0, 0, 0, 8)
            };

            // Add colored underline
            label.Paint += (s, e) =>
            {
                var lbl = s as Label;
                using (var pen = new Pen(accentColor, 3))
                {
                    e.Graphics.DrawLine(pen, 0, lbl.Height - 5, lbl.Width, lbl.Height - 5);
                }
            };

            return label;
        }

        // Toggle connection active state
        private void ToggleConnection(int connectionNumber)
        {
            _activeConnections[connectionNumber] = !_activeConnections[connectionNumber];
            RefreshUI();
        }

        // Refresh UI based on current state
        private void RefreshUI()
        {
            // This will be implemented by the designer to rebuild panels
        }

        // Event: Import Prego button clicked
        private void OnImportPregoClick(object sender, EventArgs e)
        {
            MessageBox.Show("Import Prego functionality will be integrated here", "CHART Header Tool",
                MessageBoxButtons.OK, MessageBoxIcon.Information);
        }

        // Event: Run All Headers button clicked
        private void OnRunAllHeadersClick(object sender, EventArgs e)
        {
            MessageBox.Show("Running all header configurations...", "CHART Header Tool",
                MessageBoxButtons.OK, MessageBoxIcon.Information);
        }
    }
}
