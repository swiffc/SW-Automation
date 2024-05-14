namespace Bundle
{
    partial class BundleUI
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
            this.tWidth = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.tSideFrameTHK = new System.Windows.Forms.TextBox();
            this.bBundle = new System.Windows.Forms.Button();
            this.delete_Toggle = new System.Windows.Forms.CheckBox();
            this.save_Toggle = new System.Windows.Forms.CheckBox();
            this.createDrawing_Toggle = new System.Windows.Forms.CheckBox();
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.tabPage1 = new System.Windows.Forms.TabPage();
            this.tabPage2 = new System.Windows.Forms.TabPage();
            this.tDepth = new System.Windows.Forms.TextBox();
            this.label3 = new System.Windows.Forms.Label();
            this.tabControl1.SuspendLayout();
            this.tabPage1.SuspendLayout();
            this.tabPage2.SuspendLayout();
            this.SuspendLayout();
            // 
            // tWidth
            // 
            this.tWidth.Location = new System.Drawing.Point(3, 6);
            this.tWidth.Name = "tWidth";
            this.tWidth.Size = new System.Drawing.Size(100, 20);
            this.tWidth.TabIndex = 0;
            this.tWidth.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            this.tWidth.TextChanged += new System.EventHandler(this.tWidth_TextChanged);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(109, 9);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(35, 13);
            this.label1.TabIndex = 1;
            this.label1.Text = "Width";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(288, 35);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(82, 13);
            this.label2.TabIndex = 3;
            this.label2.Text = "SideFrame THK";
            // 
            // tSideFrameTHK
            // 
            this.tSideFrameTHK.Location = new System.Drawing.Point(182, 32);
            this.tSideFrameTHK.Name = "tSideFrameTHK";
            this.tSideFrameTHK.Size = new System.Drawing.Size(100, 20);
            this.tSideFrameTHK.TabIndex = 2;
            this.tSideFrameTHK.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            this.tSideFrameTHK.Leave += new System.EventHandler(this.tSideFrameTHK_Leave);
            // 
            // bBundle
            // 
            this.bBundle.Location = new System.Drawing.Point(180, 292);
            this.bBundle.Name = "bBundle";
            this.bBundle.Size = new System.Drawing.Size(75, 23);
            this.bBundle.TabIndex = 4;
            this.bBundle.Text = "Bundle";
            this.bBundle.UseVisualStyleBackColor = true;
            this.bBundle.Click += new System.EventHandler(this.bBundle_Click);
            // 
            // delete_Toggle
            // 
            this.delete_Toggle.AutoSize = true;
            this.delete_Toggle.Location = new System.Drawing.Point(6, 52);
            this.delete_Toggle.Name = "delete_Toggle";
            this.delete_Toggle.Size = new System.Drawing.Size(128, 17);
            this.delete_Toggle.TabIndex = 38;
            this.delete_Toggle.Text = "Delete Uneeded Files";
            this.delete_Toggle.UseVisualStyleBackColor = true;
            this.delete_Toggle.CheckedChanged += new System.EventHandler(this.delete_Toggle_CheckedChanged);
            // 
            // save_Toggle
            // 
            this.save_Toggle.AutoSize = true;
            this.save_Toggle.Location = new System.Drawing.Point(6, 29);
            this.save_Toggle.Name = "save_Toggle";
            this.save_Toggle.Size = new System.Drawing.Size(76, 17);
            this.save_Toggle.TabIndex = 37;
            this.save_Toggle.Text = "Auto Save";
            this.save_Toggle.UseVisualStyleBackColor = true;
            this.save_Toggle.CheckedChanged += new System.EventHandler(this.save_Toggle_CheckedChanged);
            // 
            // createDrawing_Toggle
            // 
            this.createDrawing_Toggle.AutoSize = true;
            this.createDrawing_Toggle.Location = new System.Drawing.Point(6, 6);
            this.createDrawing_Toggle.Name = "createDrawing_Toggle";
            this.createDrawing_Toggle.Size = new System.Drawing.Size(118, 17);
            this.createDrawing_Toggle.TabIndex = 36;
            this.createDrawing_Toggle.Text = "Create Drawing File";
            this.createDrawing_Toggle.UseVisualStyleBackColor = true;
            this.createDrawing_Toggle.CheckedChanged += new System.EventHandler(this.createDrawing_Toggle_CheckedChanged);
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.tabPage1);
            this.tabControl1.Controls.Add(this.tabPage2);
            this.tabControl1.Location = new System.Drawing.Point(12, 12);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(412, 274);
            this.tabControl1.TabIndex = 39;
            // 
            // tabPage1
            // 
            this.tabPage1.Controls.Add(this.tDepth);
            this.tabPage1.Controls.Add(this.label3);
            this.tabPage1.Controls.Add(this.tWidth);
            this.tabPage1.Controls.Add(this.label1);
            this.tabPage1.Controls.Add(this.tSideFrameTHK);
            this.tabPage1.Controls.Add(this.label2);
            this.tabPage1.Location = new System.Drawing.Point(4, 22);
            this.tabPage1.Name = "tabPage1";
            this.tabPage1.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage1.Size = new System.Drawing.Size(404, 248);
            this.tabPage1.TabIndex = 0;
            this.tabPage1.Text = "Bundle";
            this.tabPage1.UseVisualStyleBackColor = true;
            // 
            // tabPage2
            // 
            this.tabPage2.Controls.Add(this.createDrawing_Toggle);
            this.tabPage2.Controls.Add(this.delete_Toggle);
            this.tabPage2.Controls.Add(this.save_Toggle);
            this.tabPage2.Location = new System.Drawing.Point(4, 22);
            this.tabPage2.Name = "tabPage2";
            this.tabPage2.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage2.Size = new System.Drawing.Size(404, 248);
            this.tabPage2.TabIndex = 1;
            this.tabPage2.Text = "Advanced";
            this.tabPage2.UseVisualStyleBackColor = true;
            // 
            // tDepth
            // 
            this.tDepth.Location = new System.Drawing.Point(182, 6);
            this.tDepth.Name = "tDepth";
            this.tDepth.Size = new System.Drawing.Size(100, 20);
            this.tDepth.TabIndex = 4;
            this.tDepth.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            this.tDepth.TextChanged += new System.EventHandler(this.tDepth_TextChanged);
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(288, 9);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(89, 13);
            this.label3.TabIndex = 5;
            this.label3.Text = "SideFrame Depth";
            // 
            // BundleUI
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(434, 322);
            this.Controls.Add(this.tabControl1);
            this.Controls.Add(this.bBundle);
            this.Name = "BundleUI";
            this.Text = "Bundle";
            this.Load += new System.EventHandler(this.BundleUI_Load);
            this.tabControl1.ResumeLayout(false);
            this.tabPage1.ResumeLayout(false);
            this.tabPage1.PerformLayout();
            this.tabPage2.ResumeLayout(false);
            this.tabPage2.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.TextBox tWidth;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox tSideFrameTHK;
        private System.Windows.Forms.Button bBundle;
        private System.Windows.Forms.CheckBox delete_Toggle;
        private System.Windows.Forms.CheckBox save_Toggle;
        private System.Windows.Forms.CheckBox createDrawing_Toggle;
        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.TabPage tabPage1;
        private System.Windows.Forms.TabPage tabPage2;
        private System.Windows.Forms.TextBox tDepth;
        private System.Windows.Forms.Label label3;
    }
}

