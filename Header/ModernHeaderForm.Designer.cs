using System;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;

namespace HDR
{
    partial class ModernHeaderForm
    {
        private System.ComponentModel.IContainer components = null;

        // Main controls
        private TabControl mainTabControl;
        private TabPage boxTab;
        private TabPage connectionsTab;
        
        // Header panel controls
        private Panel headerPanel;
        private Label titleLabel;
        private Label subtitleLabel;
        private Button importPregoButton;
        private TextBox modelTextBox;
        private TextBox modelTypeTextBox;
        
        // Connection controls
        private Panel connectionIndicatorPanel;
        private Panel optionsPanel;
        private CheckBox stainlessCheckBox;
        private CheckBox headersOutsideCheckBox;
        
        // Box tab content
        private Panel boxContentPanel;
        private FlowLayoutPanel measurementFlowPanel;
        
        // Connections tab content
        private Panel connectionsContentPanel;
        private Panel inputModePanel;
        private RadioButton traditionalRadio;
        private RadioButton singleConnectionRadio;
        private Panel connectionSelectorPanel;
        
        // Footer
        private Button runAllHeadersButton;

        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            this.SuspendLayout();
            
            // Form settings - Dark theme
            this.AutoScaleDimensions = new SizeF(8F, 17F);
            this.AutoScaleMode = AutoScaleMode.Font;
            this.ClientSize = new Size(1600, 900);
            this.Text = "CHART Header Tool";
            this.StartPosition = FormStartPosition.CenterScreen;
            this.Font = new Font("Segoe UI", 9.75F);
            this.BackColor = ColorTranslator.FromHtml("#0F172A"); // Dark slate background
            this.Padding = new Padding(0);
            this.WindowState = FormWindowState.Normal; // Don't maximize
            this.MinimumSize = new Size(1400, 800);
            
            // Initialize all controls
            InitializeHeaderPanel();
            InitializeTabControl();
            InitializeBoxTab();
            InitializeConnectionsTab();
            InitializeFooter();
            
            // Add controls to form
            this.Controls.Add(runAllHeadersButton);
            this.Controls.Add(mainTabControl);
            this.Controls.Add(headerPanel);
            
            this.ResumeLayout(false);
        }

        private void InitializeHeaderPanel()
        {
            headerPanel = new Panel
            {
                Dock = DockStyle.Top,
                Height = 90,
                BackColor = ColorTranslator.FromHtml("#1E293B"), // Dark header
                Padding = new Padding(30, 15, 30, 15)
            };

            // Title with gradient effect
            titleLabel = new Label
            {
                Text = "CHART Header Tool",
                Font = new Font("Segoe UI", 20F, FontStyle.Bold),
                ForeColor = ColorTranslator.FromHtml("#F1F5F9"), // Light text
                Location = new Point(30, 18),
                AutoSize = true
            };

            subtitleLabel = new Label
            {
                Text = "Designed for use with Prego 3.3.2",
                Font = new Font("Segoe UI", 9F),
                ForeColor = ColorTranslator.FromHtml("#94A3B8"), // Muted text
                Location = new Point(30, 50),
                AutoSize = true
            };

            // Import Prego button with accent color
            importPregoButton = CreateModernButton("Import Prego", ColorTranslator.FromHtml("#3B82F6"), OnImportPregoClick);
            importPregoButton.Location = new Point(headerPanel.Width - 200, 20);
            importPregoButton.Size = new Size(160, 50);
            importPregoButton.Anchor = AnchorStyles.Top | AnchorStyles.Right;

            // Model inputs - inline compact design
            var modelLabel = new Label
            {
                Text = "Model",
                Font = new Font("Segoe UI", 9F, FontStyle.Bold),
                ForeColor = ColorTranslator.FromHtml("#CBD5E1"),
                Location = new Point(headerPanel.Width - 520, 25),
                AutoSize = true,
                Anchor = AnchorStyles.Top | AnchorStyles.Right
            };

            modelTextBox = new TextBox
            {
                Text = "M000",
                Font = new Font("Segoe UI", 10F),
                Location = new Point(headerPanel.Width - 450, 20),
                Size = new Size(100, 32),
                BorderStyle = BorderStyle.FixedSingle,
                BackColor = ColorTranslator.FromHtml("#334155"),
                ForeColor = ColorTranslator.FromHtml("#F1F5F9"),
                Anchor = AnchorStyles.Top | AnchorStyles.Right
            };

            var typeLabel = new Label
            {
                Text = "Type",
                Font = new Font("Segoe UI", 9F, FontStyle.Bold),
                ForeColor = ColorTranslator.FromHtml("#CBD5E1"),
                Location = new Point(headerPanel.Width - 330, 25),
                AutoSize = true,
                Anchor = AnchorStyles.Top | AnchorStyles.Right
            };

            modelTypeTextBox = new TextBox
            {
                Text = "A",
                Font = new Font("Segoe UI", 10F),
                Location = new Point(headerPanel.Width - 280, 20),
                Size = new Size(60, 32),
                BorderStyle = BorderStyle.FixedSingle,
                BackColor = ColorTranslator.FromHtml("#334155"),
                ForeColor = ColorTranslator.FromHtml("#F1F5F9"),
                Anchor = AnchorStyles.Top | AnchorStyles.Right
            };

            headerPanel.Controls.AddRange(new Control[] 
            { 
                titleLabel, subtitleLabel, importPregoButton, 
                modelLabel, modelTextBox, typeLabel, modelTypeTextBox 
            });
        }

        private void InitializeTabControl()
        {
            mainTabControl = new TabControl
            {
                Dock = DockStyle.Fill,
                Font = new Font("Segoe UI", 10F, FontStyle.Bold),
                Padding = new Point(20, 10),
                Margin = new Padding(0)
            };

            mainTabControl.DrawMode = TabDrawMode.OwnerDrawFixed;
            mainTabControl.DrawItem += TabControl_DrawItem;

            boxTab = new TabPage("Box") { BackColor = ColorTranslator.FromHtml("#0F172A") };
            connectionsTab = new TabPage("Connections") { BackColor = ColorTranslator.FromHtml("#0F172A") };

            mainTabControl.TabPages.Add(boxTab);
            mainTabControl.TabPages.Add(connectionsTab);
        }

        private void TabControl_DrawItem(object sender, DrawItemEventArgs e)
        {
            var tabControl = sender as TabControl;
            var tabPage = tabControl.TabPages[e.Index];
            var tabRect = tabControl.GetTabRect(e.Index);

            // Dark theme colors
            var isSelected = (e.State & DrawItemState.Selected) == DrawItemState.Selected;
            var backColor = isSelected ? ColorTranslator.FromHtml("#1E293B") : ColorTranslator.FromHtml("#0F172A");
            var textColor = isSelected ? ColorTranslator.FromHtml("#3B82F6") : ColorTranslator.FromHtml("#64748B");

            // Fill background
            using (var brush = new SolidBrush(backColor))
            {
                e.Graphics.FillRectangle(brush, tabRect);
            }

            // Draw accent line for selected tab
            if (isSelected)
            {
                using (var pen = new Pen(ColorTranslator.FromHtml("#3B82F6"), 3))
                {
                    e.Graphics.DrawLine(pen, 
                        tabRect.Left, tabRect.Bottom - 2,
                        tabRect.Right, tabRect.Bottom - 2);
                }
            }

            // Draw text
            var textRect = new RectangleF(tabRect.X, tabRect.Y + 4, tabRect.Width, tabRect.Height - 4);
            using (var brush = new SolidBrush(textColor))
            {
                var sf = new StringFormat { Alignment = StringAlignment.Center, LineAlignment = StringAlignment.Center };
                e.Graphics.DrawString(tabPage.Text, tabControl.Font, brush, textRect, sf);
            }
        }

        private void InitializeBoxTab()
        {
            // Connection indicators (at top)
            InitializeConnectionIndicators();
            
            // Options panel (below connection indicators)
            InitializeOptionsPanel();
            
            // Measurements panel (fills rest)
            InitializeMeasurementsPanel();

            // Add in reverse order for Dock.Top to work correctly
            boxTab.Controls.Add(measurementFlowPanel);
            boxTab.Controls.Add(optionsPanel);
            boxTab.Controls.Add(connectionIndicatorPanel);
        }

        private void InitializeConnectionIndicators()
        {
            connectionIndicatorPanel = new Panel
            {
                Dock = DockStyle.Top,
                Height = 85,
                BackColor = ColorTranslator.FromHtml("#1E293B"),
                Padding = new Padding(25, 15, 25, 15),
                Margin = new Padding(0)
            };

            var titleLabel = new Label
            {
                Text = "ACTIVE CONNECTIONS",
                Font = new Font("Segoe UI", 10F, FontStyle.Bold),
                ForeColor = ColorTranslator.FromHtml("#CBD5E1"),
                Location = new Point(25, 15),
                AutoSize = true
            };

            var buttonPanel = new FlowLayoutPanel
            {
                Location = new Point(connectionIndicatorPanel.Width - 450, 10),
                Size = new Size(420, 60),
                Anchor = AnchorStyles.Top | AnchorStyles.Right,
                FlowDirection = FlowDirection.LeftToRight,
                WrapContents = false
            };

            // Create connection toggle buttons with modern design
            for (int i = 0; i < _connectionNumbers.Length; i++)
            {
                int connNum = _connectionNumbers[i];
                var btn = new Button
                {
                    Text = connNum.ToString(),
                    Size = new Size(65, 55),
                    Font = new Font("Segoe UI", 11F, FontStyle.Bold),
                    FlatStyle = FlatStyle.Flat,
                    Cursor = Cursors.Hand,
                    Tag = connNum
                };

                UpdateConnectionButtonStyle(btn, _activeConnections[connNum]);
                btn.Click += (s, e) =>
                {
                    var button = s as Button;
                    int num = (int)button.Tag;
                    _activeConnections[num] = !_activeConnections[num];
                    UpdateConnectionButtonStyle(button, _activeConnections[num]);
                    
                    // Refresh both tabs
                    RefreshBoxTab();
                    RefreshConnectionsView();
                };

                buttonPanel.Controls.Add(btn);
            }

            var hintLabel = new Label
            {
                Text = "Toggle connections • Green = Active • Gray = Disabled",
                Font = new Font("Segoe UI", 8F),
                ForeColor = ColorTranslator.FromHtml("#64748B"),
                Location = new Point(25, 55),
                AutoSize = true
            };

            connectionIndicatorPanel.Controls.AddRange(new Control[] { titleLabel, buttonPanel, hintLabel });
        }

        private void UpdateConnectionButtonStyle(Button btn, bool isActive)
        {
            if (isActive)
            {
                btn.BackColor = ColorTranslator.FromHtml("#10B981");
                btn.ForeColor = Color.White;
                btn.FlatAppearance.BorderColor = ColorTranslator.FromHtml("#059669");
                btn.FlatAppearance.BorderSize = 2;
            }
            else
            {
                btn.BackColor = ColorTranslator.FromHtml("#374151");
                btn.ForeColor = ColorTranslator.FromHtml("#6B7280");
                btn.FlatAppearance.BorderColor = ColorTranslator.FromHtml("#4B5563");
                btn.FlatAppearance.BorderSize = 1;
            }
        }

        private void InitializeOptionsPanel()
        {
            optionsPanel = new Panel
            {
                Dock = DockStyle.Top,
                Height = 70,
                BackColor = ColorTranslator.FromHtml("#1E293B"),
                Padding = new Padding(25, 12, 25, 12),
                Margin = new Padding(0)
            };

            var titleLabel = new Label
            {
                Text = "OPTIONS",
                Font = new Font("Segoe UI", 10F, FontStyle.Bold),
                ForeColor = ColorTranslator.FromHtml("#CBD5E1"),
                Location = new Point(25, 12),
                AutoSize = true
            };

            stainlessCheckBox = new CheckBox
            {
                Text = "Stainless Steel",
                Font = new Font("Segoe UI", 10F, FontStyle.Bold),
                ForeColor = ColorTranslator.FromHtml("#E2E8F0"),
                Location = new Point(25, 38),
                AutoSize = true,
                Cursor = Cursors.Hand,
                FlatStyle = FlatStyle.Flat
            };

            headersOutsideCheckBox = new CheckBox
            {
                Text = "Headers Outside The Frames",
                Font = new Font("Segoe UI", 10F, FontStyle.Bold),
                ForeColor = ColorTranslator.FromHtml("#E2E8F0"),
                Location = new Point(220, 38),
                AutoSize = true,
                Cursor = Cursors.Hand,
                FlatStyle = FlatStyle.Flat
            };

            optionsPanel.Controls.AddRange(new Control[] { titleLabel, stainlessCheckBox, headersOutsideCheckBox });
        }

        private void InitializeMeasurementsPanel()
        {
            measurementFlowPanel = new FlowLayoutPanel
            {
                Dock = DockStyle.Fill,
                AutoScroll = true,
                FlowDirection = FlowDirection.TopDown,
                WrapContents = false,
                Padding = new Padding(0, 20, 0, 20),
                BackColor = ColorTranslator.FromHtml("#0F172A")
            };

            PopulateMeasurementSections();
        }

        private void RefreshBoxTab()
        {
            measurementFlowPanel.Controls.Clear();
            PopulateMeasurementSections();
        }

        private void PopulateMeasurementSections()
        {
            // Add measurement sections
            AddMeasurementSection("Box Dimensions (Wet)", ColorTranslator.FromHtml("#3B82F6"), new System.Collections.Generic.List<Tuple<string, string>>
            {
                Tuple.Create("Width", "boxWidth"),
                Tuple.Create("Height", "boxHeight")
            });

            AddMeasurementSection("Thickness (THK)", ColorTranslator.FromHtml("#7C3AED"), new System.Collections.Generic.List<Tuple<string, string>>
            {
                Tuple.Create("Tubesheet", "thkTubesheet"),
                Tuple.Create("Plugsheet", "thkPlugsheet"),
                Tuple.Create("Top/Bottom Plate", "thkTopBottomPlate"),
                Tuple.Create("End Plate", "thkEndPlate")
            });

            AddMeasurementSection("Width", ColorTranslator.FromHtml("#F97316"), new System.Collections.Generic.List<Tuple<string, string>>
            {
                Tuple.Create("Tubesheet", "widthTubesheet"),
                Tuple.Create("Plugsheet", "widthPlugsheet"),
                Tuple.Create("Top/Bottom Plate", "widthTopBottomPlate"),
                Tuple.Create("End Plate", "widthEndPlate")
            });

            AddMeasurementSection("Length", ColorTranslator.FromHtml("#14B8A6"), new System.Collections.Generic.List<Tuple<string, string>>
            {
                Tuple.Create("Tubesheet", "lengthTubesheet"),
                Tuple.Create("Plugsheet", "lengthPlugsheet"),
                Tuple.Create("Top/Bottom Plate", "lengthTopBottomPlate")
            });

            AddMeasurementSection("Diameter", ColorTranslator.FromHtml("#EC4899"), new System.Collections.Generic.List<Tuple<string, string>>
            {
                Tuple.Create("Tube Holes", "diameterTubeHoles")
            });

            AddMeasurementSection("Offset", ColorTranslator.FromHtml("#06B6D4"), new System.Collections.Generic.List<Tuple<string, string>>
            {
                Tuple.Create("Y-OFFSET Tube Row 1", "yOffsetTubeRow1"),
                Tuple.Create("X-OFFSET Tube Rows (Odd)", "xOffsetTubeRowsOdd"),
                Tuple.Create("X-OFFSET Tube Rows (Even)", "xOffsetTubeRowsEven")
            });

            AddMeasurementSection("Count", ColorTranslator.FromHtml("#6366F1"), new System.Collections.Generic.List<Tuple<string, string>>
            {
                Tuple.Create("Row 1", "countRow1"),
                Tuple.Create("Row 2", "countRow2"),
                Tuple.Create("Row 3", "countRow3"),
                Tuple.Create("Row 4", "countRow4"),
                Tuple.Create("Row 5", "countRow5"),
                Tuple.Create("Row 6", "countRow6"),
                Tuple.Create("Row 7", "countRow7"),
                Tuple.Create("Row 8", "countRow8"),
                Tuple.Create("Row 9", "countRow9"),
                Tuple.Create("Row 10", "countRow10"),
                Tuple.Create("Row 11", "countRow11"),
                Tuple.Create("Row 12", "countRow12")
            });

            AddMeasurementSection("Pitch", ColorTranslator.FromHtml("#F59E0B"), new System.Collections.Generic.List<Tuple<string, string>>
            {
                Tuple.Create("H-PITCH Row Odd", "hPitchRowOdd"),
                Tuple.Create("H-PITCH Row Even", "hPitchRowEven"),
                Tuple.Create("V-PITCH Rows 1 and 2", "vPitchRows1And2"),
                Tuple.Create("V-PITCH Rows 2 and 3", "vPitchRows2And3"),
                Tuple.Create("V-PITCH Rows 3 and 4", "vPitchRows3And4"),
                Tuple.Create("V-PITCH Rows 4 and 5", "vPitchRows4And5"),
                Tuple.Create("V-PITCH Rows 5 and 6", "vPitchRows5And6"),
                Tuple.Create("V-PITCH Rows 6 and 7", "vPitchRows6And7"),
                Tuple.Create("V-PITCH Rows 7 and 8", "vPitchRows7And8"),
                Tuple.Create("V-PITCH Rows 8 and 9", "vPitchRows8And9"),
                Tuple.Create("V-PITCH Rows 9 and 10", "vPitchRows9And10"),
                Tuple.Create("V-PITCH Rows 10 and 11", "vPitchRows10And11"),
                Tuple.Create("V-PITCH Rows 11 and 12", "vPitchRows11And12")
            });

            AddMeasurementSection("Stiffener", ColorTranslator.FromHtml("#10B981"), new System.Collections.Generic.List<Tuple<string, string>>
            {
                Tuple.Create("THK Stiffener", "thkStiffener"),
                Tuple.Create("WIDTH Stiffener", "widthStiffener"),
                Tuple.Create("OFFSET Stiffener", "offsetStiffener")
            });

            AddMeasurementSection("Window (Stiffener)", ColorTranslator.FromHtml("#0EA5E9"), new System.Collections.Generic.List<Tuple<string, string>>
            {
                Tuple.Create("WINDOW Width", "windowWidthStiffener"),
                Tuple.Create("WINDOW Length", "windowLengthStiffener"),
                Tuple.Create("WINDOW Quantity", "windowQuantityStiffener"),
                Tuple.Create("WINDOW Spacing", "windowSpacingStiffener"),
                Tuple.Create("WINDOW Offset", "windowOffsetStiffener")
            });

            AddMeasurementSection("Location (Stiffener)", ColorTranslator.FromHtml("#F43F5E"), new System.Collections.Generic.List<Tuple<string, string>>
            {
                Tuple.Create("LOCATION Below Tube Row#", "locationBelowTubeRow"),
                Tuple.Create("DISTANCE Below Row", "distanceBelowRow"),
                Tuple.Create("LOCATION 2 Below Tube Row#", "location2BelowTubeRow"),
                Tuple.Create("DISTANCE 2 Below Row", "distance2BelowRow"),
                Tuple.Create("LOCATION 3 Below Tube Row#", "location3BelowTubeRow"),
                Tuple.Create("DISTANCE 3 Below Row", "distance3BelowRow")
            });

            AddMeasurementSection("Partition", ColorTranslator.FromHtml("#8B5CF6"), new System.Collections.Generic.List<Tuple<string, string>>
            {
                Tuple.Create("THK Partition", "thkPartition"),
                Tuple.Create("WIDTH Partition", "widthPartition"),
                Tuple.Create("LOCATION Below Tube Row#", "locationBelowTubeRowPartition"),
                Tuple.Create("DISTANCE Below Row", "distanceBelowRowPartition"),
                Tuple.Create("LOCATION 2 Below Tube Row#", "location2BelowTubeRowPartition")
            });
        }

        private void AddMeasurementSection(string title, Color accentColor, System.Collections.Generic.List<Tuple<string, string>> fields)
        {
            var sectionPanel = new Panel
            {
                Width = this.ClientSize.Width - 80,
                AutoSize = true,
                BackColor = ColorTranslator.FromHtml("#1E293B"), // Dark card
                Padding = new Padding(0),
                Margin = new Padding(20, 0, 20, 15),
                Tag = true // Expanded state
            };

            // Collapsible header
            var headerPanel = new Panel
            {
                Width = sectionPanel.Width,
                Height = 55,
                BackColor = ColorTranslator.FromHtml("#334155"),
                Cursor = Cursors.Hand,
                Tag = sectionPanel
            };

            // Accent stripe
            headerPanel.Paint += (s, e) =>
            {
                using (var brush = new SolidBrush(accentColor))
                {
                    e.Graphics.FillRectangle(brush, 0, 0, 5, headerPanel.Height);
                }
            };

            var titleLabel = new Label
            {
                Text = title.ToUpper(),
                Font = new Font("Segoe UI", 11F, FontStyle.Bold),
                ForeColor = ColorTranslator.FromHtml("#F1F5F9"),
                Location = new Point(25, 18),
                AutoSize = true,
                Cursor = Cursors.Hand
            };

            var countLabel = new Label
            {
                Text = $"{fields.Count} fields",
                Font = new Font("Segoe UI", 9F),
                ForeColor = ColorTranslator.FromHtml("#94A3B8"),
                AutoSize = true,
                Cursor = Cursors.Hand
            };
            countLabel.Location = new Point(25 + titleLabel.PreferredWidth + 15, 20);

            var expandIcon = new Label
            {
                Text = "v",
                Font = new Font("Segoe UI", 14F, FontStyle.Bold),
                ForeColor = accentColor,
                Size = new Size(30, 30),
                TextAlign = ContentAlignment.MiddleCenter,
                Cursor = Cursors.Hand,
                Tag = "expanded"
            };
            expandIcon.Location = new Point(headerPanel.Width - 50, 12);

            headerPanel.Controls.AddRange(new Control[] { titleLabel, countLabel, expandIcon });

            // Content panel
            var contentPanel = new Panel
            {
                Width = sectionPanel.Width,
                AutoSize = true,
                BackColor = ColorTranslator.FromHtml("#1E293B"),
                Padding = new Padding(20, 10, 20, 10),
                Visible = true
            };

            int yPos = 5;
            foreach (var field in fields)
            {
                var row = CreateMeasurementRow(field.Item1, field.Item2, accentColor);
                row.Location = new Point(0, yPos);
                contentPanel.Controls.Add(row);
                yPos += 42;
            }

            contentPanel.Height = yPos + 5;

            // Click to toggle
            EventHandler toggleCollapse = (s, e) =>
            {
                bool isExpanded = (string)expandIcon.Tag == "expanded";
                contentPanel.Visible = !isExpanded;
                expandIcon.Text = isExpanded ? ">" : "v";
                expandIcon.Tag = isExpanded ? "collapsed" : "expanded";
                sectionPanel.Height = isExpanded ? 55 : (55 + contentPanel.Height);
            };

            headerPanel.Click += toggleCollapse;
            titleLabel.Click += toggleCollapse;
            countLabel.Click += toggleCollapse;
            expandIcon.Click += toggleCollapse;

            sectionPanel.Controls.Add(headerPanel);
            sectionPanel.Controls.Add(contentPanel);
            contentPanel.Location = new Point(0, 55);
            sectionPanel.Height = 55 + contentPanel.Height;

            measurementFlowPanel.Controls.Add(sectionPanel);
        }

        private Panel CreateMeasurementRow(string label, string fieldName, Color accentColor)
        {
            var rowPanel = new Panel
            {
                Width = this.ClientSize.Width - 120,
                Height = 38,
                BackColor = ColorTranslator.FromHtml("#1E293B")
            };

            var lblText = new Label
            {
                Text = label,
                Font = new Font("Segoe UI", 9.5F),
                ForeColor = ColorTranslator.FromHtml("#CBD5E1"),
                Location = new Point(0, 10),
                Size = new Size(240, 20)
            };
            rowPanel.Controls.Add(lblText);

            // Create 6 textboxes with smart sizing
            int availableWidth = rowPanel.Width - 260;
            int boxWidth = (availableWidth - 50) / 6;

            for (int i = 0; i < 6; i++)
            {
                var textBox = CreateModernTextBox(fieldName, i, _activeConnections[_connectionNumbers[i]]);
                textBox.Location = new Point(260 + (i * (boxWidth + 10)), 4);
                textBox.Size = new Size(boxWidth, 30);
                textBox.BackColor = _activeConnections[_connectionNumbers[i]] 
                    ? ColorTranslator.FromHtml("#334155")
                    : ColorTranslator.FromHtml("#1E293B");
                textBox.ForeColor = _activeConnections[_connectionNumbers[i]]
                    ? ColorTranslator.FromHtml("#F1F5F9")
                    : ColorTranslator.FromHtml("#64748B");
                textBox.BorderStyle = BorderStyle.FixedSingle;
                
                // Add subtle accent glow when focused
                textBox.Enter += (s, e) =>
                {
                    ((TextBox)s).BackColor = ControlPaint.Light(accentColor, 0.8f);
                };
                textBox.Leave += (s, e) =>
                {
                    var tb = (TextBox)s;
                    var tag = tb.Tag as Tuple<string, int>;
                    tb.BackColor = _activeConnections[_connectionNumbers[tag.Item2]]
                        ? ColorTranslator.FromHtml("#334155")
                        : ColorTranslator.FromHtml("#1E293B");
                };
                
                rowPanel.Controls.Add(textBox);
            }

            return rowPanel;
        }

        private void InitializeConnectionsTab()
        {
            connectionsContentPanel = new Panel
            {
                Dock = DockStyle.Fill,
                AutoScroll = true,
                BackColor = ColorTranslator.FromHtml("#0F172A") // Dark background
            };

            // Input mode selector
            InitializeInputModePanel();

            connectionsTab.Controls.Add(inputModePanel);
            connectionsTab.Controls.Add(connectionsContentPanel);
        }

        private void InitializeInputModePanel()
        {
            inputModePanel = new Panel
            {
                Dock = DockStyle.Top,
                Height = 65,
                BackColor = ColorTranslator.FromHtml("#1E293B"), // Dark panel
                Padding = new Padding(20, 10, 20, 10)
            };

            var titleLabel = new Label
            {
                Text = "CHOOSE INPUT METHOD",
                Font = new Font("Segoe UI", 10F, FontStyle.Bold),
                ForeColor = ColorTranslator.FromHtml("#CBD5E1"),
                Location = new Point(25, 15),
                AutoSize = true
            };

            var modePanel = new Panel
            {
                Location = new Point(inputModePanel.Width - 380, 10),
                Size = new Size(350, 50),
                BackColor = ColorTranslator.FromHtml("#334155"),
                Anchor = AnchorStyles.Top | AnchorStyles.Right
            };

            traditionalRadio = new RadioButton
            {
                Text = "Traditional View",
                Font = new Font("Segoe UI", 10F, FontStyle.Bold),
                ForeColor = ColorTranslator.FromHtml("#E2E8F0"),
                Location = new Point(15, 15),
                AutoSize = true,
                Checked = true,
                Cursor = Cursors.Hand,
                FlatStyle = FlatStyle.Flat
            };

            singleConnectionRadio = new RadioButton
            {
                Text = "Single Connection",
                Font = new Font("Segoe UI", 10F, FontStyle.Bold),
                ForeColor = ColorTranslator.FromHtml("#E2E8F0"),
                Location = new Point(185, 15),
                AutoSize = true,
                Cursor = Cursors.Hand,
                FlatStyle = FlatStyle.Flat
            };

            traditionalRadio.CheckedChanged += (s, e) =>
            {
                if (traditionalRadio.Checked)
                {
                    _inputMode = "traditional";
                    RefreshConnectionsView();
                }
            };

            singleConnectionRadio.CheckedChanged += (s, e) =>
            {
                if (singleConnectionRadio.Checked)
                {
                    _inputMode = "single";
                    RefreshConnectionsView();
                }
            };

            modePanel.Controls.AddRange(new Control[] { traditionalRadio, singleConnectionRadio });

            var hintLabel = new Label
            {
                Text = "Traditional: Fill one row at a time across all connections",
                Font = new Font("Segoe UI", 8.5F),
                ForeColor = ColorTranslator.FromHtml("#64748B"),
                Location = new Point(25, 55),
                AutoSize = true
            };

            inputModePanel.Controls.AddRange(new Control[] { titleLabel, modePanel, hintLabel });
            
            // Initialize with traditional view
            RefreshConnectionsView();
        }

        private void RefreshConnectionsView()
        {
            connectionsContentPanel.Controls.Clear();
            
            if (_inputMode == "traditional")
            {
                BuildTraditionalConnectionsView();
            }
            else
            {
                BuildSingleConnectionView();
            }
        }

        private void BuildTraditionalConnectionsView()
        {
            var mainPanel = new Panel
            {
                AutoSize = true,
                AutoScroll = false,
                BackColor = ColorTranslator.FromHtml("#0F172A"), // Dark background
                Padding = new Padding(20),
                Location = new Point(0, 0),
                Width = this.ClientSize.Width - 40
            };

            // Header section with colored column indicators
            var headerPanel = new Panel
            {
                Width = mainPanel.Width - 40,
                Height = 85,
                BackColor = ColorTranslator.FromHtml("#1E293B"), // Dark card
                Margin = new Padding(0, 0, 0, 15),
                Location = new Point(0, 0)
            };

            var locationLabel = new Label
            {
                Text = "TL - LOCATION",
                Font = new Font("Segoe UI", 10F, FontStyle.Bold),
                ForeColor = ColorTranslator.FromHtml("#CBD5E1"),
                Location = new Point(20, 15),
                AutoSize = true
            };

            var locationSubLabel = new Label
            {
                Text = "In Bundle",
                Font = new Font("Segoe UI", 8.5F),
                ForeColor = ColorTranslator.FromHtml("#64748B"),
                Location = new Point(20, 40),
                AutoSize = true
            };

            headerPanel.Controls.Add(locationLabel);
            headerPanel.Controls.Add(locationSubLabel);

            // Column headers with colors
            var columnHeaderPanel = new Panel
            {
                Location = new Point(240, 10),
                Size = new Size(1250, 50)
            };

            for (int i = 0; i < 6; i++)
            {
                var headerBox = new Panel
                {
                    Location = new Point(i * 205, 0),
                    Size = new Size(195, 50),
                    BackColor = _connectionColors[i]
                };

                var numLabel = new Label
                {
                    Text = _connectionNumbers[i].ToString(),
                    Font = new Font("Segoe UI", 9F, FontStyle.Bold),
                    ForeColor = Color.White,
                    Location = new Point(0, 5),
                    Size = new Size(195, 20),
                    TextAlign = ContentAlignment.MiddleCenter
                };

                var nameLabel = new Label
                {
                    Text = _connectionNames[i],
                    Font = new Font("Segoe UI", 11F, FontStyle.Bold),
                    ForeColor = Color.White,
                    Location = new Point(0, 22),
                    Size = new Size(195, 25),
                    TextAlign = ContentAlignment.MiddleCenter
                };

                headerBox.Controls.Add(numLabel);
                headerBox.Controls.Add(nameLabel);
                columnHeaderPanel.Controls.Add(headerBox);
            }

            headerPanel.Controls.Add(columnHeaderPanel);

            // TL Dropdown row
            var tlPanel = new Panel
            {
                Location = new Point(0, 70),
                Size = new Size(1550, 40)
            };

            for (int i = 0; i < 6; i++)
            {
                var combo = CreateModernComboBox(_activeConnections[_connectionNumbers[i]]);
                combo.Location = new Point(240 + (i * 205), 5);
                combo.Size = new Size(195, 30);
                combo.BackColor = ControlPaint.Light(_connectionColors[i], 0.9f);
                tlPanel.Controls.Add(combo);
            }

            headerPanel.Controls.Add(tlPanel);
            mainPanel.Controls.Add(headerPanel);

            // Connection data rows
            var dataPanel = new Panel
            {
                Width = mainPanel.Width - 40,
                AutoSize = true,
                BackColor = ColorTranslator.FromHtml("#1E293B"), // Dark card
                Padding = new Padding(20),
                Margin = new Padding(0, 0, 0, 15),
                Location = new Point(0, 100) // Position below header panel (85px height + 15px margin)
            };

            int yPos = 10;

            // Main connection fields
            AddConnectionDataRow(dataPanel, ref yPos, "COUNT", "count");
            AddConnectionDataRow(dataPanel, ref yPos, "SPACING", "spacing");
            AddConnectionDataRow(dataPanel, ref yPos, "OFFSET", "offset", "From Box Center");
            AddConnectionDataRow(dataPanel, ref yPos, "PROJECTION", "projection", "Flange Face To Box");
            AddConnectionDataRowWithDropdown(dataPanel, ref yPos, "EXTENSION", "extension", "Type");

            yPos += 20; // Spacer

            // Dimensional fields
            AddConnectionDataRow(dataPanel, ref yPos, "O", "o");
            AddConnectionDataRow(dataPanel, ref yPos, "Q", "q");
            AddConnectionDataRow(dataPanel, ref yPos, "R", "r");
            AddConnectionDataRow(dataPanel, ref yPos, "X", "x");
            AddConnectionDataRow(dataPanel, ref yPos, "RD", "rd");
            AddConnectionDataRow(dataPanel, ref yPos, "NB", "nb");
            AddConnectionDataRow(dataPanel, ref yPos, "DB", "db");
            AddConnectionDataRow(dataPanel, ref yPos, "BC", "bc");
            AddConnectionDataRow(dataPanel, ref yPos, "YY", "yy");
            AddConnectionDataRow(dataPanel, ref yPos, "OD", "od");
            AddConnectionDataRow(dataPanel, ref yPos, "WALL", "wall");

            yPos += 20; // Spacer

            AddConnectionDataRow(dataPanel, ref yPos, "PARTNO", "partno", "Flange");

            dataPanel.Height = yPos + 20;
            mainPanel.Controls.Add(dataPanel);
            mainPanel.Height = 130 + dataPanel.Height + 20;

            connectionsContentPanel.Controls.Add(mainPanel);
        }

        private void AddConnectionDataRow(Panel parent, ref int yPos, string label, string fieldName, string sublabel = "")
        {
            var rowPanel = new Panel
            {
                Location = new Point(0, yPos),
                Size = new Size(1500, 40)
            };

            var lblText = new Label
            {
                Text = label,
                Font = new Font("Segoe UI", 9.5F, FontStyle.Bold),
                ForeColor = ColorTranslator.FromHtml("#CBD5E1"), // Light text for dark background
                Location = new Point(0, 10),
                Size = new Size(220, 20)
            };

            if (!string.IsNullOrEmpty(sublabel))
            {
                var lblSub = new Label
                {
                    Text = sublabel,
                    Font = new Font("Segoe UI", 8F, FontStyle.Italic),
                    ForeColor = ColorTranslator.FromHtml("#94A3B8"), // Muted light text
                    Location = new Point(0, 25),
                    Size = new Size(220, 15)
                };
                rowPanel.Controls.Add(lblSub);
            }

            rowPanel.Controls.Add(lblText);

            // Create 6 textboxes with color-coded backgrounds
            for (int i = 0; i < 6; i++)
            {
                var textBox = CreateModernTextBox(fieldName, i, _activeConnections[_connectionNumbers[i]]);
                textBox.Location = new Point(240 + (i * 205), 5);
                textBox.Size = new Size(195, 30);
                
                if (_activeConnections[_connectionNumbers[i]])
                {
                    textBox.BackColor = ControlPaint.Light(_connectionColors[i], 0.95f);
                    textBox.BorderStyle = BorderStyle.FixedSingle;
                }
                
                rowPanel.Controls.Add(textBox);
            }

            parent.Controls.Add(rowPanel);
            yPos += 45;
        }

        private void AddConnectionDataRowWithDropdown(Panel parent, ref int yPos, string label, string fieldName, string sublabel = "")
        {
            var rowPanel = new Panel
            {
                Location = new Point(0, yPos),
                Size = new Size(1500, 40)
            };

            var lblText = new Label
            {
                Text = label,
                Font = new Font("Segoe UI", 9.5F, FontStyle.Bold),
                ForeColor = ColorTranslator.FromHtml("#CBD5E1"), // Light text for dark background
                Location = new Point(0, 10),
                Size = new Size(220, 20)
            };

            if (!string.IsNullOrEmpty(sublabel))
            {
                var lblSub = new Label
                {
                    Text = sublabel,
                    Font = new Font("Segoe UI", 8F, FontStyle.Italic),
                    ForeColor = ColorTranslator.FromHtml("#94A3B8"), // Muted light text
                    Location = new Point(0, 25),
                    Size = new Size(220, 15)
                };
                rowPanel.Controls.Add(lblSub);
            }

            rowPanel.Controls.Add(lblText);

            // Create 6 comboboxes with color-coded backgrounds
            for (int i = 0; i < 6; i++)
            {
                var combo = CreateModernComboBox(_activeConnections[_connectionNumbers[i]]);
                combo.Location = new Point(240 + (i * 205), 5);
                combo.Size = new Size(195, 30);
                
                if (_activeConnections[_connectionNumbers[i]])
                {
                    combo.BackColor = ControlPaint.Light(_connectionColors[i], 0.95f);
                }
                
                rowPanel.Controls.Add(combo);
            }

            parent.Controls.Add(rowPanel);
            yPos += 45;
        }

        private void BuildSingleConnectionView()
        {
            var scrollPanel = new Panel
            {
                Dock = DockStyle.Fill,
                AutoScroll = true,
                BackColor = ColorTranslator.FromHtml("#0F172A"), // Dark background
                Padding = new Padding(10)
            };

            // Find first active connection
            int activeIndex = 0;
            for (int i = 0; i < _connectionNumbers.Length; i++)
            {
                if (_activeConnections[_connectionNumbers[i]])
                {
                    activeIndex = i;
                    break;
                }
            }

            _selectedConnectionIndex = activeIndex;
            int connNum = _connectionNumbers[activeIndex];
            string connName = _connectionNames[activeIndex];
            Color connColor = _connectionColors[activeIndex];

            // Connection selector buttons
            var selectorPanel = new Panel
            {
                Width = 1200,
                Height = 100,
                BackColor = ColorTranslator.FromHtml("#1E293B"), // Dark card
                Padding = new Padding(20),
                Margin = new Padding(0, 0, 0, 15)
            };

            var selectorLabel = new Label
            {
                Text = "Select Connection to Fill:",
                Font = new Font("Segoe UI", 10F, FontStyle.Bold),
                ForeColor = ColorTranslator.FromHtml("#CBD5E1"), // Light text
                Location = new Point(10, 10),
                AutoSize = true
            };

            var buttonFlowPanel = new FlowLayoutPanel
            {
                Location = new Point(10, 40),
                Size = new Size(1100, 50),
                FlowDirection = FlowDirection.LeftToRight,
                WrapContents = false
            };

            for (int i = 0; i < _connectionNumbers.Length; i++)
            {
                int index = i; // Capture for closure
                int num = _connectionNumbers[i];
                
                var btn = new Button
                {
                    Text = $"{num} - {_connectionNames[i]}",
                    Size = new Size(180, 45),
                    Font = new Font("Segoe UI", 10F, FontStyle.Bold),
                    FlatStyle = FlatStyle.Flat,
                    Cursor = Cursors.Hand,
                    Tag = index,
                    Enabled = _activeConnections[num]
                };

                if (!_activeConnections[num])
                {
                    btn.BackColor = ColorTranslator.FromHtml("#F1F5F9");
                    btn.ForeColor = ColorTranslator.FromHtml("#CBD5E1");
                }
                else if (index == activeIndex)
                {
                    btn.BackColor = _connectionColors[i];
                    btn.ForeColor = Color.White;
                    btn.FlatAppearance.BorderSize = 3;
                    btn.FlatAppearance.BorderColor = ControlPaint.Dark(_connectionColors[i], 0.2f);
                }
                else
                {
                    btn.BackColor = ControlPaint.Light(_connectionColors[i], 0.9f);
                    btn.ForeColor = ColorTranslator.FromHtml("#475569");
                    btn.FlatAppearance.BorderSize = 1;
                }

                btn.Click += (s, e) =>
                {
                    _selectedConnectionIndex = index;
                    BuildSingleConnectionView(); // Rebuild with new selection
                };

                buttonFlowPanel.Controls.Add(btn);
            }

            selectorPanel.Controls.Add(selectorLabel);
            selectorPanel.Controls.Add(buttonFlowPanel);
            scrollPanel.Controls.Add(selectorPanel);

            // Main connection form
            var formPanel = new Panel
            {
                Width = 1200,
                AutoSize = true,
                BackColor = ColorTranslator.FromHtml("#1E293B"), // Dark card
                Padding = new Padding(30),
                Margin = new Padding(0, 0, 0, 15)
            };

            // Header with connection info
            var titleLabel = new Label
            {
                Text = $"Connection {connNum}: {connName}",
                Font = new Font("Segoe UI", 16F, FontStyle.Bold),
                ForeColor = connColor,
                Location = new Point(10, 10),
                AutoSize = true
            };

            var progressLabel = new Label
            {
                Text = $"{activeIndex + 1} of 6",
                Font = new Font("Segoe UI", 10F, FontStyle.Bold),
                ForeColor = ColorTranslator.FromHtml("#10B981"),
                BackColor = ColorTranslator.FromHtml("#064E3B"), // Dark green background
                Location = new Point(1050, 15),
                Size = new Size(80, 30),
                TextAlign = ContentAlignment.MiddleCenter
            };

            var subtitleLabel = new Label
            {
                Text = "Fill all fields for this connection below",
                Font = new Font("Segoe UI", 9F),
                ForeColor = ColorTranslator.FromHtml("#94A3B8"), // Muted light text
                Location = new Point(10, 45),
                AutoSize = true
            };

            formPanel.Controls.Add(titleLabel);
            formPanel.Controls.Add(progressLabel);
            formPanel.Controls.Add(subtitleLabel);

            int yPosition = 80;

            // TL and Location section
            var tlPanel = CreateSingleFieldPanel("TL Dropdown", yPosition, connColor, true);
            formPanel.Controls.Add(tlPanel);
            yPosition += 60;

            var locPanel = CreateSingleFieldPanel("Location", yPosition, connColor, false, "In Bundle");
            formPanel.Controls.Add(locPanel);
            yPosition += 60;

            // Main fields
            yPosition = AddSingleConnectionField(formPanel, "COUNT", "count", activeIndex, yPosition, connColor);
            yPosition = AddSingleConnectionField(formPanel, "SPACING", "spacing", activeIndex, yPosition, connColor);
            yPosition = AddSingleConnectionField(formPanel, "OFFSET", "offset", activeIndex, yPosition, connColor, "From Box Center");
            yPosition = AddSingleConnectionField(formPanel, "PROJECTION", "projection", activeIndex, yPosition, connColor, "Flange Face To Box");
            yPosition = AddSingleConnectionField(formPanel, "EXTENSION", "extension", activeIndex, yPosition, connColor, "Type", true);

            yPosition += 20; // Spacer

            yPosition = AddSingleConnectionField(formPanel, "O", "o", activeIndex, yPosition, connColor);
            yPosition = AddSingleConnectionField(formPanel, "Q", "q", activeIndex, yPosition, connColor);
            yPosition = AddSingleConnectionField(formPanel, "R", "r", activeIndex, yPosition, connColor);
            yPosition = AddSingleConnectionField(formPanel, "X", "x", activeIndex, yPosition, connColor);
            yPosition = AddSingleConnectionField(formPanel, "RD", "rd", activeIndex, yPosition, connColor);
            yPosition = AddSingleConnectionField(formPanel, "NB", "nb", activeIndex, yPosition, connColor);
            yPosition = AddSingleConnectionField(formPanel, "DB", "db", activeIndex, yPosition, connColor);
            yPosition = AddSingleConnectionField(formPanel, "BC", "bc", activeIndex, yPosition, connColor);
            yPosition = AddSingleConnectionField(formPanel, "YY", "yy", activeIndex, yPosition, connColor);
            yPosition = AddSingleConnectionField(formPanel, "OD", "od", activeIndex, yPosition, connColor);
            yPosition = AddSingleConnectionField(formPanel, "WALL", "wall", activeIndex, yPosition, connColor);

            yPosition += 20; // Spacer

            yPosition = AddSingleConnectionField(formPanel, "PARTNO", "partno", activeIndex, yPosition, connColor, "Flange");

            // Tip section
            var tipPanel = new Panel
            {
                Location = new Point(10, yPosition),
                Size = new Size(1100, 60),
                BackColor = ColorTranslator.FromHtml("#1E3A5F"), // Dark blue background
                Padding = new Padding(15)
            };

            tipPanel.Paint += (s, e) =>
            {
                using (var pen = new Pen(ColorTranslator.FromHtml("#3B82F6"), 4))
                {
                    e.Graphics.DrawLine(pen, 0, 0, 0, tipPanel.Height);
                }
            };

            var tipLabel = new Label
            {
                Text = "Tip: Press Tab to move between fields quickly. All your inputs are auto-saved.",
                Font = new Font("Segoe UI", 9F),
                ForeColor = ColorTranslator.FromHtml("#93C5FD"), // Light blue text
                Location = new Point(20, 15),
                Size = new Size(1050, 30)
            };

            tipPanel.Controls.Add(tipLabel);
            formPanel.Controls.Add(tipPanel);

            formPanel.Height = yPosition + 80;
            scrollPanel.Controls.Add(formPanel);

            connectionsContentPanel.Controls.Add(scrollPanel);
        }

        private Panel CreateSingleFieldPanel(string label, int yPos, Color accentColor, bool isDropdown, string defaultValue = "")
        {
            var panel = new Panel
            {
                Location = new Point(10, yPos),
                Size = new Size(500, 50)
            };

            var lblText = new Label
            {
                Text = label,
                Font = new Font("Segoe UI", 9.5F, FontStyle.Bold),
                ForeColor = ColorTranslator.FromHtml("#CBD5E1"), // Light text for dark background
                Location = new Point(0, 0),
                AutoSize = true
            };

            Control inputControl;
            if (isDropdown)
            {
                inputControl = CreateModernComboBox(true);
                inputControl.Size = new Size(300, 30);
            }
            else
            {
                inputControl = new TextBox
                {
                    Font = new Font("Segoe UI", 10F),
                    BorderStyle = BorderStyle.FixedSingle,
                    Size = new Size(300, 30),
                    Text = defaultValue
                };
            }

            inputControl.Location = new Point(0, 22);
            inputControl.BackColor = ControlPaint.Light(accentColor, 0.95f);

            panel.Controls.Add(lblText);
            panel.Controls.Add(inputControl);

            return panel;
        }

        private int AddSingleConnectionField(Panel parent, string label, string fieldName, int connIndex, 
            int yPos, Color accentColor, string sublabel = "", bool isDropdown = false)
        {
            var rowPanel = new Panel
            {
                Location = new Point(10, yPos),
                Size = new Size(1100, 50),
                BackColor = ColorTranslator.FromHtml("#0F172A"), // Dark background for field rows
                Padding = new Padding(10)
            };

            var lblText = new Label
            {
                Text = label,
                Font = new Font("Segoe UI", 9.5F, FontStyle.Bold),
                ForeColor = ColorTranslator.FromHtml("#CBD5E1"), // Light text
                Location = new Point(10, 10),
                Size = new Size(200, 20)
            };

            if (!string.IsNullOrEmpty(sublabel))
            {
                var lblSub = new Label
                {
                    Text = sublabel,
                    Font = new Font("Segoe UI", 8F, FontStyle.Italic),
                    ForeColor = ColorTranslator.FromHtml("#94A3B8"), // Muted light text
                    Location = new Point(10, 28),
                    Size = new Size(200, 15)
                };
                rowPanel.Controls.Add(lblSub);
            }

            rowPanel.Controls.Add(lblText);

            Control inputControl;
            if (isDropdown)
            {
                inputControl = CreateModernComboBox(true);
            }
            else
            {
                inputControl = CreateModernTextBox(fieldName, connIndex, true);
            }

            inputControl.Location = new Point(220, 8);
            inputControl.Size = new Size(850, 30);
            inputControl.BackColor = ControlPaint.Light(accentColor, 0.95f);

            rowPanel.Controls.Add(inputControl);
            parent.Controls.Add(rowPanel);

            return yPos + 55;
        }

        private void InitializeFooter()
        {
            runAllHeadersButton = new Button
            {
                Text = "? RUN ALL HEADERS",
                Font = new Font("Segoe UI", 12F, FontStyle.Bold),
                ForeColor = Color.White,
                BackColor = ColorTranslator.FromHtml("#7C3AED"),
                FlatStyle = FlatStyle.Flat,
                Dock = DockStyle.Bottom,
                Height = 65,
                Cursor = Cursors.Hand
            };

            runAllHeadersButton.FlatAppearance.BorderSize = 0;
            runAllHeadersButton.FlatAppearance.MouseOverBackColor = ColorTranslator.FromHtml("#8B5CF6");
            runAllHeadersButton.FlatAppearance.MouseDownBackColor = ColorTranslator.FromHtml("#6D28D9");
            runAllHeadersButton.Click += OnRunAllHeadersClick;
        }

        private GraphicsPath CreateRoundedRectangle(Rectangle bounds, int radius)
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

            // top left arc
            path.AddArc(arc, 180, 90);

            // top right arc
            arc.X = bounds.Right - diameter;
            path.AddArc(arc, 270, 90);

            // bottom right arc
            arc.Y = bounds.Bottom - diameter;
            path.AddArc(arc, 0, 90);

            // bottom left arc
            arc.X = bounds.Left;
            path.AddArc(arc, 90, 90);

            path.CloseFigure();
            return path;
        }
    }
}
