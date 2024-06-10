namespace SolidWorks_Add_In
{
    partial class TaskpaneHostUI
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

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(TaskpaneHostUI));
            this.PreviousSheet = new System.Windows.Forms.Button();
            this.button2 = new System.Windows.Forms.Button();
            this.PosScale = new System.Windows.Forms.Button();
            this.button4 = new System.Windows.Forms.Button();
            this.button5 = new System.Windows.Forms.Button();
            this.button6 = new System.Windows.Forms.Button();
            this.button7 = new System.Windows.Forms.Button();
            this.button8 = new System.Windows.Forms.Button();
            this.button9 = new System.Windows.Forms.Button();
            this.button10 = new System.Windows.Forms.Button();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.pictureBox1 = new System.Windows.Forms.PictureBox();
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            this.button1 = new System.Windows.Forms.Button();
            this.button3 = new System.Windows.Forms.Button();
            this.btn_HoodUI = new System.Windows.Forms.Button();
            this.plenum_button = new System.Windows.Forms.Button();
            this.launch25 = new System.Windows.Forms.Button();
            this.label4 = new System.Windows.Forms.Label();
            this.button_ImportConfigFile = new System.Windows.Forms.Button();
            this.button_ExportConfigFile = new System.Windows.Forms.Button();
            this.panel1 = new System.Windows.Forms.Panel();
            this.launchMM = new System.Windows.Forms.Button();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).BeginInit();
            this.panel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // PreviousSheet
            // 
            this.PreviousSheet.BackColor = System.Drawing.SystemColors.Control;
            this.PreviousSheet.ForeColor = System.Drawing.SystemColors.ControlText;
            this.PreviousSheet.Location = new System.Drawing.Point(6, 16);
            this.PreviousSheet.Name = "PreviousSheet";
            this.PreviousSheet.Size = new System.Drawing.Size(97, 23);
            this.PreviousSheet.TabIndex = 0;
            this.PreviousSheet.Text = "Previous Sheet";
            this.PreviousSheet.UseVisualStyleBackColor = false;
            this.PreviousSheet.Click += new System.EventHandler(this.PreviousSheet_Click);
            // 
            // button2
            // 
            this.button2.BackColor = System.Drawing.SystemColors.Control;
            this.button2.ForeColor = System.Drawing.SystemColors.ControlText;
            this.button2.Location = new System.Drawing.Point(6, 45);
            this.button2.Name = "button2";
            this.button2.Size = new System.Drawing.Size(97, 23);
            this.button2.TabIndex = 1;
            this.button2.Text = "Next Sheet";
            this.button2.UseVisualStyleBackColor = false;
            this.button2.Click += new System.EventHandler(this.NextSheet_Click);
            // 
            // PosScale
            // 
            this.PosScale.BackColor = System.Drawing.SystemColors.Control;
            this.PosScale.ForeColor = System.Drawing.SystemColors.ControlText;
            this.PosScale.Location = new System.Drawing.Point(6, 115);
            this.PosScale.Name = "PosScale";
            this.PosScale.Size = new System.Drawing.Size(97, 23);
            this.PosScale.TabIndex = 2;
            this.PosScale.Text = "Position + Scale";
            this.PosScale.UseVisualStyleBackColor = false;
            this.PosScale.Click += new System.EventHandler(this.PositionScale_Click);
            // 
            // button4
            // 
            this.button4.BackColor = System.Drawing.SystemColors.Control;
            this.button4.ForeColor = System.Drawing.SystemColors.ControlText;
            this.button4.Location = new System.Drawing.Point(6, 86);
            this.button4.Name = "button4";
            this.button4.Size = new System.Drawing.Size(97, 23);
            this.button4.TabIndex = 3;
            this.button4.Text = "AutoBalloon";
            this.button4.UseVisualStyleBackColor = false;
            this.button4.Click += new System.EventHandler(this.AutoBalloon_Click);
            // 
            // button5
            // 
            this.button5.BackColor = System.Drawing.SystemColors.Control;
            this.button5.ForeColor = System.Drawing.SystemColors.ControlText;
            this.button5.Location = new System.Drawing.Point(6, 144);
            this.button5.Name = "button5";
            this.button5.Size = new System.Drawing.Size(97, 23);
            this.button5.TabIndex = 4;
            this.button5.Text = "Align Dimensions";
            this.button5.UseVisualStyleBackColor = false;
            this.button5.Click += new System.EventHandler(this.AlignDimensions_Click);
            // 
            // button6
            // 
            this.button6.BackColor = System.Drawing.SystemColors.Control;
            this.button6.ForeColor = System.Drawing.SystemColors.ControlText;
            this.button6.Location = new System.Drawing.Point(6, 173);
            this.button6.Name = "button6";
            this.button6.Size = new System.Drawing.Size(97, 23);
            this.button6.TabIndex = 5;
            this.button6.Text = "Delete Dangling";
            this.button6.UseVisualStyleBackColor = false;
            this.button6.Click += new System.EventHandler(this.DeleteDangling_Click);
            // 
            // button7
            // 
            this.button7.BackColor = System.Drawing.SystemColors.Control;
            this.button7.ForeColor = System.Drawing.SystemColors.ControlText;
            this.button7.Location = new System.Drawing.Point(109, 99);
            this.button7.Name = "button7";
            this.button7.Size = new System.Drawing.Size(97, 23);
            this.button7.TabIndex = 6;
            this.button7.Text = "Sheet Cleaner";
            this.button7.UseVisualStyleBackColor = false;
            this.button7.Click += new System.EventHandler(this.SheetCleaner_Click);
            // 
            // button8
            // 
            this.button8.BackColor = System.Drawing.SystemColors.Control;
            this.button8.ForeColor = System.Drawing.SystemColors.ControlText;
            this.button8.Location = new System.Drawing.Point(109, 155);
            this.button8.Name = "button8";
            this.button8.Size = new System.Drawing.Size(97, 23);
            this.button8.TabIndex = 7;
            this.button8.Text = "Drawing Cleaner";
            this.button8.UseVisualStyleBackColor = false;
            this.button8.Click += new System.EventHandler(this.DrawingCleaner_Click);
            // 
            // button9
            // 
            this.button9.Location = new System.Drawing.Point(6, 327);
            this.button9.Name = "button9";
            this.button9.Size = new System.Drawing.Size(200, 23);
            this.button9.TabIndex = 10;
            this.button9.Text = "Count Holes On Selected Faces";
            this.button9.UseVisualStyleBackColor = true;
            this.button9.Click += new System.EventHandler(this.CountHoles_Click);
            // 
            // button10
            // 
            this.button10.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.button10.Location = new System.Drawing.Point(6, 517);
            this.button10.Name = "button10";
            this.button10.Size = new System.Drawing.Size(200, 53);
            this.button10.TabIndex = 11;
            this.button10.Text = "28 (Walkway)";
            this.button10.UseVisualStyleBackColor = true;
            this.button10.Click += new System.EventHandler(this.WalkwayTool_Click);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.ForeColor = System.Drawing.SystemColors.Control;
            this.label1.Location = new System.Drawing.Point(3, 0);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(75, 13);
            this.label1.TabIndex = 12;
            this.label1.Text = "Drawing Tools";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.ForeColor = System.Drawing.SystemColors.Control;
            this.label2.Location = new System.Drawing.Point(3, 311);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(65, 13);
            this.label2.TabIndex = 13;
            this.label2.Text = "Model Tools";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.ForeColor = System.Drawing.SystemColors.Control;
            this.label3.Location = new System.Drawing.Point(3, 501);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(89, 13);
            this.label3.TabIndex = 14;
            this.label3.Text = "Automation Tools";
            // 
            // pictureBox1
            // 
            this.pictureBox1.Image = ((System.Drawing.Image)(resources.GetObject("pictureBox1.Image")));
            this.pictureBox1.Location = new System.Drawing.Point(16, 15);
            this.pictureBox1.Name = "pictureBox1";
            this.pictureBox1.Size = new System.Drawing.Size(32, 32);
            this.pictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage;
            this.pictureBox1.TabIndex = 15;
            this.pictureBox1.TabStop = false;
            this.pictureBox1.Click += new System.EventHandler(this.pictureBox1_Click);
            this.pictureBox1.MouseLeave += new System.EventHandler(this.Version_MouseLeave);
            this.pictureBox1.MouseHover += new System.EventHandler(this.Version_MouseHover);
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(6, 214);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(200, 26);
            this.button1.TabIndex = 16;
            this.button1.Text = "Split Multi-Sheet Drawing";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Click += new System.EventHandler(this.SplitDrawing_Click);
            // 
            // button3
            // 
            this.button3.Location = new System.Drawing.Point(6, 246);
            this.button3.Name = "button3";
            this.button3.Size = new System.Drawing.Size(200, 23);
            this.button3.TabIndex = 17;
            this.button3.Text = "Migrate Drawing Sheets";
            this.button3.UseVisualStyleBackColor = true;
            this.button3.Click += new System.EventHandler(this.MigrateDrawing_Click);
            // 
            // btn_HoodUI
            // 
            this.btn_HoodUI.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btn_HoodUI.Location = new System.Drawing.Point(6, 576);
            this.btn_HoodUI.Name = "btn_HoodUI";
            this.btn_HoodUI.Size = new System.Drawing.Size(200, 53);
            this.btn_HoodUI.TabIndex = 18;
            this.btn_HoodUI.Text = "3 (Hood)";
            this.btn_HoodUI.UseVisualStyleBackColor = true;
            this.btn_HoodUI.Click += new System.EventHandler(this.btn_HoodUI_Click);
            // 
            // plenum_button
            // 
            this.plenum_button.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.plenum_button.Location = new System.Drawing.Point(6, 635);
            this.plenum_button.Name = "plenum_button";
            this.plenum_button.Size = new System.Drawing.Size(200, 53);
            this.plenum_button.TabIndex = 19;
            this.plenum_button.Text = "5 (Plenum)";
            this.plenum_button.UseVisualStyleBackColor = true;
            this.plenum_button.Click += new System.EventHandler(this.plenum_button_Click);
            // 
            // launch25
            // 
            this.launch25.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.launch25.Location = new System.Drawing.Point(6, 694);
            this.launch25.Name = "launch25";
            this.launch25.Size = new System.Drawing.Size(200, 53);
            this.launch25.TabIndex = 20;
            this.launch25.Text = "25 (Sub-Structure)";
            this.launch25.UseVisualStyleBackColor = true;
            this.launch25.Click += new System.EventHandler(this.launch25_Click);
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.ForeColor = System.Drawing.SystemColors.Control;
            this.label4.Location = new System.Drawing.Point(3, 389);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(47, 13);
            this.label4.TabIndex = 22;
            this.label4.Text = "UI Tools";
            // 
            // button_ImportConfigFile
            // 
            this.button_ImportConfigFile.Location = new System.Drawing.Point(6, 405);
            this.button_ImportConfigFile.Name = "button_ImportConfigFile";
            this.button_ImportConfigFile.Size = new System.Drawing.Size(200, 23);
            this.button_ImportConfigFile.TabIndex = 21;
            this.button_ImportConfigFile.Text = "Import Config File";
            this.button_ImportConfigFile.UseVisualStyleBackColor = true;
            this.button_ImportConfigFile.Click += new System.EventHandler(this.button_ImportConfigFile_Click);
            // 
            // button_ExportConfigFile
            // 
            this.button_ExportConfigFile.Location = new System.Drawing.Point(6, 434);
            this.button_ExportConfigFile.Name = "button_ExportConfigFile";
            this.button_ExportConfigFile.Size = new System.Drawing.Size(200, 23);
            this.button_ExportConfigFile.TabIndex = 23;
            this.button_ExportConfigFile.Text = "Export Config File";
            this.button_ExportConfigFile.UseVisualStyleBackColor = true;
            this.button_ExportConfigFile.Click += new System.EventHandler(this.button_ExportConfigFile_Click);
            // 
            // panel1
            // 
            this.panel1.AutoScroll = true;
            this.panel1.Controls.Add(this.launchMM);
            this.panel1.Controls.Add(this.label1);
            this.panel1.Controls.Add(this.button_ExportConfigFile);
            this.panel1.Controls.Add(this.PreviousSheet);
            this.panel1.Controls.Add(this.label4);
            this.panel1.Controls.Add(this.button2);
            this.panel1.Controls.Add(this.button_ImportConfigFile);
            this.panel1.Controls.Add(this.PosScale);
            this.panel1.Controls.Add(this.launch25);
            this.panel1.Controls.Add(this.button4);
            this.panel1.Controls.Add(this.plenum_button);
            this.panel1.Controls.Add(this.button5);
            this.panel1.Controls.Add(this.btn_HoodUI);
            this.panel1.Controls.Add(this.button6);
            this.panel1.Controls.Add(this.button3);
            this.panel1.Controls.Add(this.button7);
            this.panel1.Controls.Add(this.button1);
            this.panel1.Controls.Add(this.button8);
            this.panel1.Controls.Add(this.button9);
            this.panel1.Controls.Add(this.label3);
            this.panel1.Controls.Add(this.button10);
            this.panel1.Controls.Add(this.label2);
            this.panel1.Location = new System.Drawing.Point(16, 53);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(232, 576);
            this.panel1.TabIndex = 24;
            // 
            // launchMM
            // 
            this.launchMM.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.launchMM.Location = new System.Drawing.Point(6, 753);
            this.launchMM.Name = "launchMM";
            this.launchMM.Size = new System.Drawing.Size(200, 53);
            this.launchMM.TabIndex = 24;
            this.launchMM.Text = "4 (Machinery Mount)";
            this.launchMM.UseVisualStyleBackColor = true;
            this.launchMM.Click += new System.EventHandler(this.launchMM_Click);
            // 
            // TaskpaneHostUI
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.SystemColors.WindowFrame;
            this.Controls.Add(this.panel1);
            this.Controls.Add(this.pictureBox1);
            this.Margin = new System.Windows.Forms.Padding(2);
            this.Name = "TaskpaneHostUI";
            this.Size = new System.Drawing.Size(270, 652);
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).EndInit();
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button PreviousSheet;
        private System.Windows.Forms.Button button2;
        private System.Windows.Forms.Button PosScale;
        private System.Windows.Forms.Button button4;
        private System.Windows.Forms.Button button5;
        private System.Windows.Forms.Button button6;
        private System.Windows.Forms.Button button7;
        private System.Windows.Forms.Button button8;
        private System.Windows.Forms.Button button9;
        private System.Windows.Forms.Button button10;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.PictureBox pictureBox1;
        private System.Windows.Forms.ToolTip toolTip1;
        private System.Windows.Forms.Button button1;
        private System.Windows.Forms.Button button3;
        private System.Windows.Forms.Button btn_HoodUI;
        private System.Windows.Forms.Button plenum_button;
        private System.Windows.Forms.Button launch25;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Button button_ImportConfigFile;
        private System.Windows.Forms.Button button_ExportConfigFile;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Button launchMM;
    }
}
