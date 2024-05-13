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
            this.SuspendLayout();
            // 
            // tWidth
            // 
            this.tWidth.Location = new System.Drawing.Point(12, 12);
            this.tWidth.Name = "tWidth";
            this.tWidth.Size = new System.Drawing.Size(100, 20);
            this.tWidth.TabIndex = 0;
            this.tWidth.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            this.tWidth.TextChanged += new System.EventHandler(this.tWidth_TextChanged);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(118, 15);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(35, 13);
            this.label1.TabIndex = 1;
            this.label1.Text = "Width";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(118, 41);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(82, 13);
            this.label2.TabIndex = 3;
            this.label2.Text = "SideFrame THK";
            // 
            // tSideFrameTHK
            // 
            this.tSideFrameTHK.Location = new System.Drawing.Point(12, 38);
            this.tSideFrameTHK.Name = "tSideFrameTHK";
            this.tSideFrameTHK.Size = new System.Drawing.Size(100, 20);
            this.tSideFrameTHK.TabIndex = 2;
            this.tSideFrameTHK.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            this.tSideFrameTHK.TextChanged += new System.EventHandler(this.tSideFrameTHK_TextChanged);
            this.tSideFrameTHK.Leave += new System.EventHandler(this.tSideFrameTHK_Leave);
            // 
            // bBundle
            // 
            this.bBundle.Location = new System.Drawing.Point(139, 89);
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
            this.delete_Toggle.Location = new System.Drawing.Point(220, 57);
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
            this.save_Toggle.Location = new System.Drawing.Point(220, 34);
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
            this.createDrawing_Toggle.Location = new System.Drawing.Point(220, 11);
            this.createDrawing_Toggle.Name = "createDrawing_Toggle";
            this.createDrawing_Toggle.Size = new System.Drawing.Size(118, 17);
            this.createDrawing_Toggle.TabIndex = 36;
            this.createDrawing_Toggle.Text = "Create Drawing File";
            this.createDrawing_Toggle.UseVisualStyleBackColor = true;
            this.createDrawing_Toggle.CheckedChanged += new System.EventHandler(this.createDrawing_Toggle_CheckedChanged);
            // 
            // fBundle
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(353, 123);
            this.Controls.Add(this.delete_Toggle);
            this.Controls.Add(this.save_Toggle);
            this.Controls.Add(this.createDrawing_Toggle);
            this.Controls.Add(this.bBundle);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.tSideFrameTHK);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.tWidth);
            this.Name = "fBundle";
            this.Text = "Bundle";
            this.Load += new System.EventHandler(this.BundleUI_Load);
            this.ResumeLayout(false);
            this.PerformLayout();

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
    }
}

