namespace HDR
{
    partial class ModernHeaderUI
    {
        private System.ComponentModel.IContainer components = null;

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
            this.mainPanel = new System.Windows.Forms.Panel();
            this.headerPanel = new System.Windows.Forms.Panel();
            this.titleLabel = new System.Windows.Forms.Label();
            this.subtitleLabel = new System.Windows.Forms.Label();
            this.closeButton = new System.Windows.Forms.Button();
            this.minimizeButton = new System.Windows.Forms.Button();
            this.contentPanel = new System.Windows.Forms.Panel();
            this.tabControl = new System.Windows.Forms.TabControl();
            this.generalTab = new System.Windows.Forms.TabPage();
            this.jobPanel = new System.Windows.Forms.Panel();
            this.jobNumberTextBox = new System.Windows.Forms.TextBox();
            this.jobLabel = new System.Windows.Forms.Label();
            this.bankTextBox = new System.Windows.Forms.TextBox();
            this.bankLabel = new System.Windows.Forms.Label();
            this.configurationTab = new System.Windows.Forms.TabPage();
            this.footerPanel = new System.Windows.Forms.Panel();
            this.importPregoButton = new System.Windows.Forms.Button();
            this.createButton = new System.Windows.Forms.Button();
            this.runButton = new System.Windows.Forms.Button();
            this.statusStrip = new System.Windows.Forms.Label();
            
            this.mainPanel.SuspendLayout();
            this.headerPanel.SuspendLayout();
            this.contentPanel.SuspendLayout();
            this.tabControl.SuspendLayout();
            this.generalTab.SuspendLayout();
            this.jobPanel.SuspendLayout();
            this.footerPanel.SuspendLayout();
            this.SuspendLayout();
            
            // 
            // mainPanel
            // 
            this.mainPanel.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(240)))), ((int)(((byte)(242)))), ((int)(((byte)(245)))));
            this.mainPanel.Controls.Add(this.headerPanel);
            this.mainPanel.Controls.Add(this.contentPanel);
            this.mainPanel.Controls.Add(this.footerPanel);
            this.mainPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.mainPanel.Location = new System.Drawing.Point(0, 0);
            this.mainPanel.Name = "mainPanel";
            this.mainPanel.Padding = new System.Windows.Forms.Padding(1);
            this.mainPanel.Size = new System.Drawing.Size(900, 700);
            this.mainPanel.TabIndex = 0;
            
            // 
            // headerPanel
            // 
            this.headerPanel.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(26)))), ((int)(((byte)(32)))), ((int)(((byte)(44)))));
            this.headerPanel.Controls.Add(this.titleLabel);
            this.headerPanel.Controls.Add(this.subtitleLabel);
            this.headerPanel.Controls.Add(this.closeButton);
            this.headerPanel.Controls.Add(this.minimizeButton);
            this.headerPanel.Dock = System.Windows.Forms.DockStyle.Top;
            this.headerPanel.Location = new System.Drawing.Point(1, 1);
            this.headerPanel.Name = "headerPanel";
            this.headerPanel.Size = new System.Drawing.Size(898, 80);
            this.headerPanel.TabIndex = 0;
            this.headerPanel.MouseDown += new System.Windows.Forms.MouseEventHandler(this.HeaderPanel_MouseDown);
            this.headerPanel.Paint += new System.Windows.Forms.PaintEventHandler(this.HeaderPanel_Paint);
            
            // 
            // titleLabel
            // 
            this.titleLabel.AutoSize = true;
            this.titleLabel.Font = new System.Drawing.Font("Segoe UI Light", 24F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point);
            this.titleLabel.ForeColor = System.Drawing.Color.White;
            this.titleLabel.Location = new System.Drawing.Point(25, 15);
            this.titleLabel.Name = "titleLabel";
            this.titleLabel.Size = new System.Drawing.Size(250, 45);
            this.titleLabel.TabIndex = 0;
            this.titleLabel.Text = "CHART Header Tool";
            this.titleLabel.MouseDown += new System.Windows.Forms.MouseEventHandler(this.HeaderPanel_MouseDown);
            
            // 
            // subtitleLabel
            // 
            this.subtitleLabel.AutoSize = true;
            this.subtitleLabel.Font = new System.Drawing.Font("Segoe UI", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point);
            this.subtitleLabel.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(156)))), ((int)(((byte)(163)))), ((int)(((byte)(175)))));
            this.subtitleLabel.Location = new System.Drawing.Point(28, 55);
            this.subtitleLabel.Name = "subtitleLabel";
            this.subtitleLabel.Size = new System.Drawing.Size(220, 15);
            this.subtitleLabel.TabIndex = 1;
            this.subtitleLabel.Text = "Automated Header Design & Configuration";
            this.subtitleLabel.MouseDown += new System.Windows.Forms.MouseEventHandler(this.HeaderPanel_MouseDown);
            
            // 
            // closeButton
            // 
            this.closeButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.closeButton.FlatAppearance.BorderSize = 0;
            this.closeButton.FlatAppearance.MouseOverBackColor = System.Drawing.Color.FromArgb(((int)(((byte)(220)))), ((int)(((byte)(53)))), ((int)(((byte)(69)))));
            this.closeButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.closeButton.Font = new System.Drawing.Font("Segoe UI", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point);
            this.closeButton.ForeColor = System.Drawing.Color.White;
            this.closeButton.Location = new System.Drawing.Point(848, 10);
            this.closeButton.Name = "closeButton";
            this.closeButton.Size = new System.Drawing.Size(40, 30);
            this.closeButton.TabIndex = 2;
            this.closeButton.Text = "?";
            this.closeButton.UseVisualStyleBackColor = false;
            this.closeButton.Click += new System.EventHandler(this.CloseButton_Click);
            
            // 
            // minimizeButton
            // 
            this.minimizeButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.minimizeButton.FlatAppearance.BorderSize = 0;
            this.minimizeButton.FlatAppearance.MouseOverBackColor = System.Drawing.Color.FromArgb(((int)(((byte)(55)))), ((int)(((byte)(65)))), ((int)(((byte)(81)))));
            this.minimizeButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.minimizeButton.Font = new System.Drawing.Font("Segoe UI", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point);
            this.minimizeButton.ForeColor = System.Drawing.Color.White;
            this.minimizeButton.Location = new System.Drawing.Point(808, 10);
            this.minimizeButton.Name = "minimizeButton";
            this.minimizeButton.Size = new System.Drawing.Size(40, 30);
            this.minimizeButton.TabIndex = 3;
            this.minimizeButton.Text = "?";
            this.minimizeButton.UseVisualStyleBackColor = false;
            this.minimizeButton.Click += new System.EventHandler(this.MinimizeButton_Click);
            
            // 
            // contentPanel
            // 
            this.contentPanel.BackColor = System.Drawing.Color.White;
            this.contentPanel.Controls.Add(this.tabControl);
            this.contentPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.contentPanel.Location = new System.Drawing.Point(1, 81);
            this.contentPanel.Margin = new System.Windows.Forms.Padding(10);
            this.contentPanel.Name = "contentPanel";
            this.contentPanel.Padding = new System.Windows.Forms.Padding(20);
            this.contentPanel.Size = new System.Drawing.Size(898, 539);
            this.contentPanel.TabIndex = 1;
            
            // 
            // tabControl
            // 
            this.tabControl.Controls.Add(this.generalTab);
            this.tabControl.Controls.Add(this.configurationTab);
            this.tabControl.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabControl.DrawMode = System.Windows.Forms.TabDrawMode.OwnerDrawFixed;
            this.tabControl.Font = new System.Drawing.Font("Segoe UI", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point);
            this.tabControl.ItemSize = new System.Drawing.Size(150, 45);
            this.tabControl.Location = new System.Drawing.Point(20, 20);
            this.tabControl.Name = "tabControl";
            this.tabControl.Padding = new System.Drawing.Point(20, 5);
            this.tabControl.SelectedIndex = 0;
            this.tabControl.Size = new System.Drawing.Size(858, 499);
            this.tabControl.SizeMode = System.Windows.Forms.TabSizeMode.Fixed;
            this.tabControl.TabIndex = 0;
            this.tabControl.DrawItem += new System.Windows.Forms.DrawItemEventHandler(this.TabControl_DrawItem);
            
            // 
            // generalTab
            // 
            this.generalTab.BackColor = System.Drawing.Color.White;
            this.generalTab.Controls.Add(this.jobPanel);
            this.generalTab.Location = new System.Drawing.Point(4, 49);
            this.generalTab.Name = "generalTab";
            this.generalTab.Padding = new System.Windows.Forms.Padding(15);
            this.generalTab.Size = new System.Drawing.Size(850, 446);
            this.generalTab.TabIndex = 0;
            this.generalTab.Text = "General";
            
            // 
            // jobPanel
            // 
            this.jobPanel.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(248)))), ((int)(((byte)(250)))), ((int)(((byte)(252)))));
            this.jobPanel.Controls.Add(this.jobLabel);
            this.jobPanel.Controls.Add(this.jobNumberTextBox);
            this.jobPanel.Controls.Add(this.bankLabel);
            this.jobPanel.Controls.Add(this.bankTextBox);
            this.jobPanel.Location = new System.Drawing.Point(15, 15);
            this.jobPanel.Name = "jobPanel";
            this.jobPanel.Padding = new System.Windows.Forms.Padding(20);
            this.jobPanel.Size = new System.Drawing.Size(400, 150);
            this.jobPanel.TabIndex = 0;
            this.jobPanel.Paint += new System.Windows.Forms.PaintEventHandler(this.Panel_Paint);
            
            // 
            // jobLabel
            // 
            this.jobLabel.AutoSize = true;
            this.jobLabel.Font = new System.Drawing.Font("Segoe UI Semibold", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point);
            this.jobLabel.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(51)))), ((int)(((byte)(65)))), ((int)(((byte)(85)))));
            this.jobLabel.Location = new System.Drawing.Point(20, 20);
            this.jobLabel.Name = "jobLabel";
            this.jobLabel.Size = new System.Drawing.Size(85, 19);
            this.jobLabel.TabIndex = 0;
            this.jobLabel.Text = "Job Number";
            
            // 
            // jobNumberTextBox
            // 
            this.jobNumberTextBox.BackColor = System.Drawing.Color.White;
            this.jobNumberTextBox.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.jobNumberTextBox.Font = new System.Drawing.Font("Segoe UI", 11F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point);
            this.jobNumberTextBox.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(15)))), ((int)(((byte)(23)))), ((int)(((byte)(42)))));
            this.jobNumberTextBox.Location = new System.Drawing.Point(23, 48);
            this.jobNumberTextBox.Name = "jobNumberTextBox";
            this.jobNumberTextBox.Size = new System.Drawing.Size(354, 20);
            this.jobNumberTextBox.TabIndex = 1;
            this.jobNumberTextBox.Enter += new System.EventHandler(this.TextBox_Enter);
            this.jobNumberTextBox.Leave += new System.EventHandler(this.TextBox_Leave);
            this.jobNumberTextBox.Paint += new System.Windows.Forms.PaintEventHandler(this.TextBox_Paint);
            
            // 
            // bankLabel
            // 
            this.bankLabel.AutoSize = true;
            this.bankLabel.Font = new System.Drawing.Font("Segoe UI Semibold", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point);
            this.bankLabel.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(51)))), ((int)(((byte)(65)))), ((int)(((byte)(85)))));
            this.bankLabel.Location = new System.Drawing.Point(20, 85);
            this.bankLabel.Name = "bankLabel";
            this.bankLabel.Size = new System.Drawing.Size(39, 19);
            this.bankLabel.TabIndex = 2;
            this.bankLabel.Text = "Bank";
            
            // 
            // bankTextBox
            // 
            this.bankTextBox.BackColor = System.Drawing.Color.White;
            this.bankTextBox.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.bankTextBox.Font = new System.Drawing.Font("Segoe UI", 11F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point);
            this.bankTextBox.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(15)))), ((int)(((byte)(23)))), ((int)(((byte)(42)))));
            this.bankTextBox.Location = new System.Drawing.Point(23, 113);
            this.bankTextBox.Name = "bankTextBox";
            this.bankTextBox.Size = new System.Drawing.Size(100, 20);
            this.bankTextBox.TabIndex = 3;
            this.bankTextBox.Enter += new System.EventHandler(this.TextBox_Enter);
            this.bankTextBox.Leave += new System.EventHandler(this.TextBox_Leave);
            
            // 
            // configurationTab
            // 
            this.configurationTab.BackColor = System.Drawing.Color.White;
            this.configurationTab.Location = new System.Drawing.Point(4, 49);
            this.configurationTab.Name = "configurationTab";
            this.configurationTab.Padding = new System.Windows.Forms.Padding(15);
            this.configurationTab.Size = new System.Drawing.Size(850, 446);
            this.configurationTab.TabIndex = 1;
            this.configurationTab.Text = "Configuration";
            
            // 
            // footerPanel
            // 
            this.footerPanel.BackColor = System.Drawing.Color.White;
            this.footerPanel.Controls.Add(this.statusStrip);
            this.footerPanel.Controls.Add(this.importPregoButton);
            this.footerPanel.Controls.Add(this.createButton);
            this.footerPanel.Controls.Add(this.runButton);
            this.footerPanel.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.footerPanel.Location = new System.Drawing.Point(1, 620);
            this.footerPanel.Name = "footerPanel";
            this.footerPanel.Padding = new System.Windows.Forms.Padding(20, 15, 20, 20);
            this.footerPanel.Size = new System.Drawing.Size(898, 79);
            this.footerPanel.TabIndex = 2;
            this.footerPanel.Paint += new System.Windows.Forms.PaintEventHandler(this.Footer_Paint);
            
            // 
            // statusStrip
            // 
            this.statusStrip.Dock = System.Windows.Forms.DockStyle.Left;
            this.statusStrip.Font = new System.Drawing.Font("Segoe UI", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point);
            this.statusStrip.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(100)))), ((int)(((byte)(116)))), ((int)(((byte)(139)))));
            this.statusStrip.Location = new System.Drawing.Point(20, 15);
            this.statusStrip.Name = "statusStrip";
            this.statusStrip.Size = new System.Drawing.Size(400, 44);
            this.statusStrip.TabIndex = 0;
            this.statusStrip.Text = "Ready";
            this.statusStrip.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            
            // 
            // importPregoButton
            // 
            this.importPregoButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.importPregoButton.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(59)))), ((int)(((byte)(130)))), ((int)(((byte)(246)))));
            this.importPregoButton.FlatAppearance.BorderSize = 0;
            this.importPregoButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.importPregoButton.Font = new System.Drawing.Font("Segoe UI Semibold", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point);
            this.importPregoButton.ForeColor = System.Drawing.Color.White;
            this.importPregoButton.Location = new System.Drawing.Point(458, 20);
            this.importPregoButton.Name = "importPregoButton";
            this.importPregoButton.Size = new System.Drawing.Size(130, 40);
            this.importPregoButton.TabIndex = 1;
            this.importPregoButton.Text = "?? Import Prego";
            this.importPregoButton.UseVisualStyleBackColor = false;
            this.importPregoButton.Click += new System.EventHandler(this.ImportPregoButton_Click);
            this.importPregoButton.Paint += new System.Windows.Forms.PaintEventHandler(this.Button_Paint);
            
            // 
            // createButton
            // 
            this.createButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.createButton.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(16)))), ((int)(((byte)(185)))), ((int)(((byte)(129)))));
            this.createButton.FlatAppearance.BorderSize = 0;
            this.createButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.createButton.Font = new System.Drawing.Font("Segoe UI Semibold", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point);
            this.createButton.ForeColor = System.Drawing.Color.White;
            this.createButton.Location = new System.Drawing.Point(598, 20);
            this.createButton.Name = "createButton";
            this.createButton.Size = new System.Drawing.Size(130, 40);
            this.createButton.TabIndex = 2;
            this.createButton.Text = "?? Create/Update";
            this.createButton.UseVisualStyleBackColor = false;
            this.createButton.Click += new System.EventHandler(this.CreateButton_Click);
            this.createButton.Paint += new System.Windows.Forms.PaintEventHandler(this.Button_Paint);
            
            // 
            // runButton
            // 
            this.runButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.runButton.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(124)))), ((int)(((byte)(58)))), ((int)(((byte)(237)))));
            this.runButton.FlatAppearance.BorderSize = 0;
            this.runButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.runButton.Font = new System.Drawing.Font("Segoe UI Semibold", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point);
            this.runButton.ForeColor = System.Drawing.Color.White;
            this.runButton.Location = new System.Drawing.Point(738, 20);
            this.runButton.Name = "runButton";
            this.runButton.Size = new System.Drawing.Size(140, 40);
            this.runButton.TabIndex = 3;
            this.runButton.Text = "?? Run";
            this.runButton.UseVisualStyleBackColor = false;
            this.runButton.Click += new System.EventHandler(this.RunButton_Click);
            this.runButton.Paint += new System.Windows.Forms.PaintEventHandler(this.Button_Paint);
            
            // 
            // ModernHeaderUI
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(26)))), ((int)(((byte)(32)))), ((int)(((byte)(44)))));
            this.ClientSize = new System.Drawing.Size(900, 700);
            this.Controls.Add(this.mainPanel);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None;
            this.Name = "ModernHeaderUI";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "CHART Header Tool";
            this.Load += new System.EventHandler(this.ModernHeaderUI_Load);
            this.Paint += new System.Windows.Forms.PaintEventHandler(this.Form_Paint);
            
            this.mainPanel.ResumeLayout(false);
            this.headerPanel.ResumeLayout(false);
            this.headerPanel.PerformLayout();
            this.contentPanel.ResumeLayout(false);
            this.tabControl.ResumeLayout(false);
            this.generalTab.ResumeLayout(false);
            this.jobPanel.ResumeLayout(false);
            this.jobPanel.PerformLayout();
            this.footerPanel.ResumeLayout(false);
            this.ResumeLayout(false);
        }

        private System.Windows.Forms.Panel mainPanel;
        private System.Windows.Forms.Panel headerPanel;
        private System.Windows.Forms.Label titleLabel;
        private System.Windows.Forms.Label subtitleLabel;
        private System.Windows.Forms.Button closeButton;
        private System.Windows.Forms.Button minimizeButton;
        private System.Windows.Forms.Panel contentPanel;
        private System.Windows.Forms.TabControl tabControl;
        private System.Windows.Forms.TabPage generalTab;
        private System.Windows.Forms.TabPage configurationTab;
        private System.Windows.Forms.Panel jobPanel;
        private System.Windows.Forms.Label jobLabel;
        private System.Windows.Forms.TextBox jobNumberTextBox;
        private System.Windows.Forms.Label bankLabel;
        private System.Windows.Forms.TextBox bankTextBox;
        private System.Windows.Forms.Panel footerPanel;
        private System.Windows.Forms.Label statusStrip;
        private System.Windows.Forms.Button importPregoButton;
        private System.Windows.Forms.Button createButton;
        private System.Windows.Forms.Button runButton;
    }
}
