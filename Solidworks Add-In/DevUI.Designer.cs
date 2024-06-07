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
            this.SuspendLayout();
            // 
            // button_ResetDefaults
            // 
            this.button_ResetDefaults.Location = new System.Drawing.Point(12, 12);
            this.button_ResetDefaults.Name = "button_ResetDefaults";
            this.button_ResetDefaults.Size = new System.Drawing.Size(89, 23);
            this.button_ResetDefaults.TabIndex = 0;
            this.button_ResetDefaults.Text = "Clear UI Cache";
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
            // DevUI
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(244, 43);
            this.Controls.Add(this.bExportTemplates);
            this.Controls.Add(this.button_ResetDefaults);
            this.Name = "DevUI";
            this.Text = "DevUI";
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button button_ResetDefaults;
        private System.Windows.Forms.Button bExportTemplates;
    }
}