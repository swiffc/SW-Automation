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
            this.tBundleWidth = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.tSideFrameTHK = new System.Windows.Forms.TextBox();
            this.bBundle = new System.Windows.Forms.Button();
            this.delete_Toggle = new System.Windows.Forms.CheckBox();
            this.save_Toggle = new System.Windows.Forms.CheckBox();
            this.createDrawing_Toggle = new System.Windows.Forms.CheckBox();
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.tabPage1 = new System.Windows.Forms.TabPage();
            this.splitContainer1 = new System.Windows.Forms.SplitContainer();
            this.panel2 = new System.Windows.Forms.Panel();
            this.client_Box = new System.Windows.Forms.TextBox();
            this.label4 = new System.Windows.Forms.Label();
            this.splitter4 = new System.Windows.Forms.Splitter();
            this.splitter3 = new System.Windows.Forms.Splitter();
            this.job_Box = new System.Windows.Forms.TextBox();
            this.bImportPrego = new System.Windows.Forms.Button();
            this.textBox_Bank = new System.Windows.Forms.TextBox();
            this.splitter1 = new System.Windows.Forms.Splitter();
            this.panel1 = new System.Windows.Forms.Panel();
            this.label10 = new System.Windows.Forms.Label();
            this.label9 = new System.Windows.Forms.Label();
            this.label7 = new System.Windows.Forms.Label();
            this.label6 = new System.Windows.Forms.Label();
            this.itemNumber_Box = new System.Windows.Forms.TextBox();
            this.label3 = new System.Windows.Forms.Label();
            this.tDepth = new System.Windows.Forms.TextBox();
            this.location_Box = new System.Windows.Forms.TextBox();
            this.initials_Box = new System.Windows.Forms.TextBox();
            this.customer_Box = new System.Windows.Forms.TextBox();
            this.purchaseOrder_Box = new System.Windows.Forms.TextBox();
            this.label8 = new System.Windows.Forms.Label();
            this.splitter2 = new System.Windows.Forms.Splitter();
            this.tabPage2 = new System.Windows.Forms.TabPage();
            this.tabControl1.SuspendLayout();
            this.tabPage1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).BeginInit();
            this.splitContainer1.Panel1.SuspendLayout();
            this.splitContainer1.Panel2.SuspendLayout();
            this.splitContainer1.SuspendLayout();
            this.panel2.SuspendLayout();
            this.panel1.SuspendLayout();
            this.tabPage2.SuspendLayout();
            this.SuspendLayout();
            // 
            // tBundleWidth
            // 
            this.tBundleWidth.Location = new System.Drawing.Point(11, 104);
            this.tBundleWidth.Name = "tBundleWidth";
            this.tBundleWidth.Size = new System.Drawing.Size(100, 20);
            this.tBundleWidth.TabIndex = 0;
            this.tBundleWidth.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            this.tBundleWidth.TextChanged += new System.EventHandler(this.tWidth_TextChanged);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.BackColor = System.Drawing.Color.White;
            this.label1.Location = new System.Drawing.Point(117, 107);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(71, 13);
            this.label1.TabIndex = 1;
            this.label1.Text = "Bundle Width";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.BackColor = System.Drawing.Color.White;
            this.label2.Location = new System.Drawing.Point(117, 159);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(82, 13);
            this.label2.TabIndex = 3;
            this.label2.Text = "SideFrame THK";
            // 
            // tSideFrameTHK
            // 
            this.tSideFrameTHK.Location = new System.Drawing.Point(11, 156);
            this.tSideFrameTHK.Name = "tSideFrameTHK";
            this.tSideFrameTHK.Size = new System.Drawing.Size(100, 20);
            this.tSideFrameTHK.TabIndex = 2;
            this.tSideFrameTHK.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            this.tSideFrameTHK.Leave += new System.EventHandler(this.tSideFrameTHK_Leave);
            // 
            // bBundle
            // 
            this.bBundle.Location = new System.Drawing.Point(189, 565);
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
            this.tabControl1.Size = new System.Drawing.Size(426, 547);
            this.tabControl1.TabIndex = 39;
            // 
            // tabPage1
            // 
            this.tabPage1.BackColor = System.Drawing.Color.LightGray;
            this.tabPage1.Controls.Add(this.splitContainer1);
            this.tabPage1.Location = new System.Drawing.Point(4, 22);
            this.tabPage1.Name = "tabPage1";
            this.tabPage1.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage1.Size = new System.Drawing.Size(418, 521);
            this.tabPage1.TabIndex = 0;
            this.tabPage1.Text = "Bundle";
            // 
            // splitContainer1
            // 
            this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer1.Location = new System.Drawing.Point(3, 3);
            this.splitContainer1.Name = "splitContainer1";
            // 
            // splitContainer1.Panel1
            // 
            this.splitContainer1.Panel1.BackColor = System.Drawing.Color.LightGray;
            this.splitContainer1.Panel1.Controls.Add(this.panel2);
            this.splitContainer1.Panel1.Controls.Add(this.label4);
            this.splitContainer1.Panel1.Controls.Add(this.splitter4);
            this.splitContainer1.Panel1.Controls.Add(this.splitter3);
            this.splitContainer1.Panel1.Controls.Add(this.job_Box);
            this.splitContainer1.Panel1.Controls.Add(this.bImportPrego);
            this.splitContainer1.Panel1.Controls.Add(this.textBox_Bank);
            this.splitContainer1.Panel1.Controls.Add(this.splitter1);
            // 
            // splitContainer1.Panel2
            // 
            this.splitContainer1.Panel2.Controls.Add(this.panel1);
            this.splitContainer1.Panel2.Controls.Add(this.label8);
            this.splitContainer1.Panel2.Controls.Add(this.splitter2);
            this.splitContainer1.Size = new System.Drawing.Size(412, 515);
            this.splitContainer1.SplitterDistance = 145;
            this.splitContainer1.TabIndex = 0;
            // 
            // panel2
            // 
            this.panel2.AutoScroll = true;
            this.panel2.BackColor = System.Drawing.Color.White;
            this.panel2.Controls.Add(this.client_Box);
            this.panel2.Controls.Add(this.initials_Box);
            this.panel2.Location = new System.Drawing.Point(3, 114);
            this.panel2.Name = "panel2";
            this.panel2.Size = new System.Drawing.Size(134, 398);
            this.panel2.TabIndex = 88;
            // 
            // client_Box
            // 
            this.client_Box.Location = new System.Drawing.Point(9, 0);
            this.client_Box.Name = "client_Box";
            this.client_Box.Size = new System.Drawing.Size(100, 20);
            this.client_Box.TabIndex = 78;
            this.client_Box.TextChanged += new System.EventHandler(this.client_Box_TextChanged);
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.BackColor = System.Drawing.Color.White;
            this.label4.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, ((System.Drawing.FontStyle)((System.Drawing.FontStyle.Bold | System.Drawing.FontStyle.Underline))), System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label4.Location = new System.Drawing.Point(38, 88);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(57, 16);
            this.label4.TabIndex = 87;
            this.label4.Text = "Manual";
            // 
            // splitter4
            // 
            this.splitter4.BackColor = System.Drawing.Color.White;
            this.splitter4.Dock = System.Windows.Forms.DockStyle.Top;
            this.splitter4.Location = new System.Drawing.Point(0, 75);
            this.splitter4.Name = "splitter4";
            this.splitter4.Size = new System.Drawing.Size(145, 440);
            this.splitter4.TabIndex = 86;
            this.splitter4.TabStop = false;
            // 
            // splitter3
            // 
            this.splitter3.Dock = System.Windows.Forms.DockStyle.Top;
            this.splitter3.Location = new System.Drawing.Point(0, 72);
            this.splitter3.Name = "splitter3";
            this.splitter3.Size = new System.Drawing.Size(145, 3);
            this.splitter3.TabIndex = 85;
            this.splitter3.TabStop = false;
            // 
            // job_Box
            // 
            this.job_Box.Location = new System.Drawing.Point(12, 10);
            this.job_Box.Name = "job_Box";
            this.job_Box.Size = new System.Drawing.Size(74, 20);
            this.job_Box.TabIndex = 76;
            this.job_Box.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            this.job_Box.TextChanged += new System.EventHandler(this.job_Box_TextChanged);
            // 
            // bImportPrego
            // 
            this.bImportPrego.Location = new System.Drawing.Point(12, 36);
            this.bImportPrego.Name = "bImportPrego";
            this.bImportPrego.Size = new System.Drawing.Size(99, 23);
            this.bImportPrego.TabIndex = 84;
            this.bImportPrego.Text = "Import Prego";
            this.bImportPrego.UseVisualStyleBackColor = true;
            this.bImportPrego.Click += new System.EventHandler(this.bImportPrego_Click);
            // 
            // textBox_Bank
            // 
            this.textBox_Bank.Location = new System.Drawing.Point(92, 10);
            this.textBox_Bank.Name = "textBox_Bank";
            this.textBox_Bank.Size = new System.Drawing.Size(20, 20);
            this.textBox_Bank.TabIndex = 83;
            this.textBox_Bank.TextChanged += new System.EventHandler(this.textBox_Bank_TextChanged);
            // 
            // splitter1
            // 
            this.splitter1.BackColor = System.Drawing.Color.White;
            this.splitter1.Dock = System.Windows.Forms.DockStyle.Top;
            this.splitter1.Location = new System.Drawing.Point(0, 0);
            this.splitter1.Name = "splitter1";
            this.splitter1.Size = new System.Drawing.Size(145, 72);
            this.splitter1.TabIndex = 0;
            this.splitter1.TabStop = false;
            // 
            // panel1
            // 
            this.panel1.AutoScroll = true;
            this.panel1.BackColor = System.Drawing.Color.White;
            this.panel1.Controls.Add(this.label10);
            this.panel1.Controls.Add(this.label9);
            this.panel1.Controls.Add(this.label7);
            this.panel1.Controls.Add(this.label6);
            this.panel1.Controls.Add(this.label2);
            this.panel1.Controls.Add(this.tBundleWidth);
            this.panel1.Controls.Add(this.itemNumber_Box);
            this.panel1.Controls.Add(this.label3);
            this.panel1.Controls.Add(this.tDepth);
            this.panel1.Controls.Add(this.tSideFrameTHK);
            this.panel1.Controls.Add(this.location_Box);
            this.panel1.Controls.Add(this.label1);
            this.panel1.Controls.Add(this.customer_Box);
            this.panel1.Controls.Add(this.purchaseOrder_Box);
            this.panel1.Location = new System.Drawing.Point(3, 36);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(257, 476);
            this.panel1.TabIndex = 86;
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.BackColor = System.Drawing.Color.White;
            this.label10.Location = new System.Drawing.Point(117, 5);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(51, 13);
            this.label10.TabIndex = 87;
            this.label10.Text = "Customer";
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.BackColor = System.Drawing.Color.White;
            this.label9.Location = new System.Drawing.Point(117, 29);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(48, 13);
            this.label9.TabIndex = 86;
            this.label9.Text = "Location";
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.BackColor = System.Drawing.Color.White;
            this.label7.Location = new System.Drawing.Point(117, 55);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(81, 13);
            this.label7.TabIndex = 85;
            this.label7.Text = "Purchase Order";
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.BackColor = System.Drawing.Color.White;
            this.label6.Location = new System.Drawing.Point(117, 81);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(67, 13);
            this.label6.TabIndex = 84;
            this.label6.Text = "Item Number";
            // 
            // itemNumber_Box
            // 
            this.itemNumber_Box.Location = new System.Drawing.Point(11, 78);
            this.itemNumber_Box.Name = "itemNumber_Box";
            this.itemNumber_Box.Size = new System.Drawing.Size(100, 20);
            this.itemNumber_Box.TabIndex = 81;
            this.itemNumber_Box.TextChanged += new System.EventHandler(this.itemNumber_Box_TextChanged);
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.BackColor = System.Drawing.Color.White;
            this.label3.Location = new System.Drawing.Point(117, 133);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(89, 13);
            this.label3.TabIndex = 5;
            this.label3.Text = "SideFrame Depth";
            // 
            // tDepth
            // 
            this.tDepth.Location = new System.Drawing.Point(11, 130);
            this.tDepth.Name = "tDepth";
            this.tDepth.Size = new System.Drawing.Size(100, 20);
            this.tDepth.TabIndex = 4;
            this.tDepth.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            this.tDepth.TextChanged += new System.EventHandler(this.tDepth_TextChanged);
            // 
            // location_Box
            // 
            this.location_Box.Location = new System.Drawing.Point(11, 26);
            this.location_Box.Name = "location_Box";
            this.location_Box.Size = new System.Drawing.Size(100, 20);
            this.location_Box.TabIndex = 79;
            this.location_Box.TextChanged += new System.EventHandler(this.location_Box_TextChanged);
            // 
            // initials_Box
            // 
            this.initials_Box.Location = new System.Drawing.Point(9, 26);
            this.initials_Box.Name = "initials_Box";
            this.initials_Box.Size = new System.Drawing.Size(100, 20);
            this.initials_Box.TabIndex = 82;
            this.initials_Box.TextChanged += new System.EventHandler(this.initials_Box_TextChanged);
            // 
            // customer_Box
            // 
            this.customer_Box.Location = new System.Drawing.Point(11, 0);
            this.customer_Box.Name = "customer_Box";
            this.customer_Box.Size = new System.Drawing.Size(100, 20);
            this.customer_Box.TabIndex = 77;
            this.customer_Box.TextChanged += new System.EventHandler(this.customer_Box_TextChanged);
            // 
            // purchaseOrder_Box
            // 
            this.purchaseOrder_Box.Location = new System.Drawing.Point(11, 52);
            this.purchaseOrder_Box.Name = "purchaseOrder_Box";
            this.purchaseOrder_Box.Size = new System.Drawing.Size(100, 20);
            this.purchaseOrder_Box.TabIndex = 80;
            this.purchaseOrder_Box.TextChanged += new System.EventHandler(this.purchaseOrder_Box_TextChanged);
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.BackColor = System.Drawing.Color.White;
            this.label8.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, ((System.Drawing.FontStyle)((System.Drawing.FontStyle.Bold | System.Drawing.FontStyle.Underline))), System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label8.Location = new System.Drawing.Point(87, 11);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(104, 16);
            this.label8.TabIndex = 85;
            this.label8.Text = "Prego Imports";
            // 
            // splitter2
            // 
            this.splitter2.BackColor = System.Drawing.Color.White;
            this.splitter2.Dock = System.Windows.Forms.DockStyle.Top;
            this.splitter2.Location = new System.Drawing.Point(0, 0);
            this.splitter2.Name = "splitter2";
            this.splitter2.Size = new System.Drawing.Size(263, 515);
            this.splitter2.TabIndex = 0;
            this.splitter2.TabStop = false;
            // 
            // tabPage2
            // 
            this.tabPage2.Controls.Add(this.createDrawing_Toggle);
            this.tabPage2.Controls.Add(this.delete_Toggle);
            this.tabPage2.Controls.Add(this.save_Toggle);
            this.tabPage2.Location = new System.Drawing.Point(4, 22);
            this.tabPage2.Name = "tabPage2";
            this.tabPage2.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage2.Size = new System.Drawing.Size(418, 521);
            this.tabPage2.TabIndex = 1;
            this.tabPage2.Text = "Advanced";
            this.tabPage2.UseVisualStyleBackColor = true;
            // 
            // BundleUI
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(448, 598);
            this.Controls.Add(this.tabControl1);
            this.Controls.Add(this.bBundle);
            this.Name = "BundleUI";
            this.Text = "Bundle";
            this.Load += new System.EventHandler(this.BundleUI_Load);
            this.tabControl1.ResumeLayout(false);
            this.tabPage1.ResumeLayout(false);
            this.splitContainer1.Panel1.ResumeLayout(false);
            this.splitContainer1.Panel1.PerformLayout();
            this.splitContainer1.Panel2.ResumeLayout(false);
            this.splitContainer1.Panel2.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).EndInit();
            this.splitContainer1.ResumeLayout(false);
            this.panel2.ResumeLayout(false);
            this.panel2.PerformLayout();
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.tabPage2.ResumeLayout(false);
            this.tabPage2.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.TextBox tBundleWidth;
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
        private System.Windows.Forms.TextBox itemNumber_Box;
        private System.Windows.Forms.TextBox textBox_Bank;
        private System.Windows.Forms.TextBox job_Box;
        private System.Windows.Forms.TextBox customer_Box;
        private System.Windows.Forms.TextBox initials_Box;
        private System.Windows.Forms.TextBox location_Box;
        private System.Windows.Forms.TextBox purchaseOrder_Box;
        private System.Windows.Forms.TextBox client_Box;
        private System.Windows.Forms.Button bImportPrego;
        private System.Windows.Forms.SplitContainer splitContainer1;
        private System.Windows.Forms.Splitter splitter1;
        private System.Windows.Forms.Splitter splitter2;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.Splitter splitter4;
        private System.Windows.Forms.Splitter splitter3;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Panel panel2;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.Label label6;
    }
}

