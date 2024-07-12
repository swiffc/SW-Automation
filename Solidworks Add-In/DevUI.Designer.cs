namespace SolidWorks_Add_In
{
    partial class DevUI
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.button_ResetDefaults = new System.Windows.Forms.Button();
            this.bExportTemplates = new System.Windows.Forms.Button();
            this.cDevMode = new System.Windows.Forms.CheckBox();
            this.button1 = new System.Windows.Forms.Button();
            this.button2 = new System.Windows.Forms.Button();
            this.button3 = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // button_ResetDefaults
            // 
            this.button_ResetDefaults.Location = new System.Drawing.Point(12, 12);
            this.button_ResetDefaults.Name = "button_ResetDefaults";
            this.button_ResetDefaults.Size = new System.Drawing.Size(100, 23);
            this.button_ResetDefaults.TabIndex = 0;
            this.button_ResetDefaults.Text = "Reset UI Config";
            this.button_ResetDefaults.UseVisualStyleBackColor = true;
            this.button_ResetDefaults.Click += new System.EventHandler(this.button_ResetDefaults_Click);
            // 
            // bExportTemplates
            // 
            this.bExportTemplates.Location = new System.Drawing.Point(118, 12);
            this.bExportTemplates.Name = "bExportTemplates";
            this.bExportTemplates.Size = new System.Drawing.Size(114, 23);
            this.bExportTemplates.TabIndex = 1;
            this.bExportTemplates.Text = "Export As Templates";
            this.bExportTemplates.UseVisualStyleBackColor = true;
            this.bExportTemplates.Click += new System.EventHandler(this.bExportTemplates_Click);
            // 
            // cDevMode
            // 
            this.cDevMode.AutoSize = true;
            this.cDevMode.Location = new System.Drawing.Point(76, 99);
            this.cDevMode.Name = "cDevMode";
            this.cDevMode.Size = new System.Drawing.Size(73, 17);
            this.cDevMode.TabIndex = 2;
            this.cDevMode.Text = "DevMode";
            this.cDevMode.UseVisualStyleBackColor = true;
            this.cDevMode.CheckedChanged += new System.EventHandler(this.cDevMode_CheckedChanged);
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(12, 41);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(100, 23);
            this.button1.TabIndex = 3;
            this.button1.Text = "Delete UI Config";
            this.button1.UseVisualStyleBackColor = true;
            // 
            // button2
            // 
            this.button2.Location = new System.Drawing.Point(118, 41);
            this.button2.Name = "button2";
            this.button2.Size = new System.Drawing.Size(114, 23);
            this.button2.TabIndex = 4;
            this.button2.Text = "Save Everything";
            this.button2.UseVisualStyleBackColor = true;
            this.button2.Click += new System.EventHandler(this.button2_Click);
            // 
            // button3
            // 
            this.button3.Location = new System.Drawing.Point(118, 70);
            this.button3.Name = "button3";
            this.button3.Size = new System.Drawing.Size(114, 23);
            this.button3.TabIndex = 5;
            this.button3.Text = "Close Everything";
            this.button3.UseVisualStyleBackColor = true;
            this.button3.Click += new System.EventHandler(this.button3_Click);
            // 
            // DevUI
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(244, 125);
            this.Controls.Add(this.button3);
            this.Controls.Add(this.button2);
            this.Controls.Add(this.button1);
            this.Controls.Add(this.cDevMode);
            this.Controls.Add(this.bExportTemplates);
            this.Controls.Add(this.button_ResetDefaults);
            this.Name = "DevUI";
            this.Text = "DevUI";
            this.Load += new System.EventHandler(this.DevUI_Load);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button button_ResetDefaults;
        private System.Windows.Forms.Button bExportTemplates;
        private System.Windows.Forms.CheckBox cDevMode;
        private System.Windows.Forms.Button button1;
        private System.Windows.Forms.Button button2;
        private System.Windows.Forms.Button button3;
    }
}