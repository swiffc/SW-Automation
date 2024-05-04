using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum
{
    partial class PlenumUI
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
            if (disposing)
                SettingsChanged -= UpdateUI;
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.tab_AdvOptions = new System.Windows.Forms.TabPage();
            this.label35 = new System.Windows.Forms.Label();
            this.textBox_SpliceOverride = new System.Windows.Forms.TextBox();
            this.button1 = new System.Windows.Forms.Button();
            this.checkBox4_delete = new System.Windows.Forms.CheckBox();
            this.checkBox3_save = new System.Windows.Forms.CheckBox();
            this.checkBox2_dwg = new System.Windows.Forms.CheckBox();
            this.leg_floor = new System.Windows.Forms.Button();
            this.jhn_floor = new System.Windows.Forms.Button();
            this.std_floor = new System.Windows.Forms.Button();
            this.label27 = new System.Windows.Forms.Label();
            this.label30 = new System.Windows.Forms.Label();
            this.label31 = new System.Windows.Forms.Label();
            this.label29 = new System.Windows.Forms.Label();
            this.label28 = new System.Windows.Forms.Label();
            this.zShift_txt = new System.Windows.Forms.TextBox();
            this.lengthAdj = new System.Windows.Forms.TextBox();
            this.txt_xShift = new System.Windows.Forms.TextBox();
            this.label25 = new System.Windows.Forms.Label();
            this.label26 = new System.Windows.Forms.Label();
            this.label24 = new System.Windows.Forms.Label();
            this.label15 = new System.Windows.Forms.Label();
            this.textBox_K1 = new System.Windows.Forms.TextBox();
            this.textBox_K = new System.Windows.Forms.TextBox();
            this.textBox_FlangeTHK = new System.Windows.Forms.TextBox();
            this.textBox_FlangeWidth = new System.Windows.Forms.TextBox();
            this.textBox_WebTHK = new System.Windows.Forms.TextBox();
            this.textBox_Depth = new System.Windows.Forms.TextBox();
            this.label14 = new System.Windows.Forms.Label();
            this.label13 = new System.Windows.Forms.Label();
            this.label12 = new System.Windows.Forms.Label();
            this.label11 = new System.Windows.Forms.Label();
            this.label10 = new System.Windows.Forms.Label();
            this.label8 = new System.Windows.Forms.Label();
            this.tab_Plenum = new System.Windows.Forms.TabPage();
            this.label34 = new System.Windows.Forms.Label();
            this.flangeGageWT_Box = new System.Windows.Forms.TextBox();
            this.label33 = new System.Windows.Forms.Label();
            this.braceHoleDiameter_Box = new System.Windows.Forms.TextBox();
            this.label32 = new System.Windows.Forms.Label();
            this.clipTHK_Box = new System.Windows.Forms.TextBox();
            this.label21 = new System.Windows.Forms.Label();
            this.mmHeight_Box = new System.Windows.Forms.TextBox();
            this.textBox_TotalColumnHeight = new System.Windows.Forms.TextBox();
            this.label6 = new System.Windows.Forms.Label();
            this.label50 = new System.Windows.Forms.Label();
            this.textBox_ClipHeight = new System.Windows.Forms.TextBox();
            this.label5 = new System.Windows.Forms.Label();
            this.comboBox_BraceType = new System.Windows.Forms.ComboBox();
            this.textBoxShipBeamHeight = new System.Windows.Forms.TextBox();
            this.label4 = new System.Windows.Forms.Label();
            this.textBox_DriveWidth = new System.Windows.Forms.TextBox();
            this.label3 = new System.Windows.Forms.Label();
            this.textBox_BraceAngle = new System.Windows.Forms.TextBox();
            this.label2 = new System.Windows.Forms.Label();
            this.label1 = new System.Windows.Forms.Label();
            this.textBox_Bank = new System.Windows.Forms.TextBox();
            this.txt_Initials = new System.Windows.Forms.TextBox();
            this.txt_JobItemNo = new System.Windows.Forms.TextBox();
            this.txt_JobPO = new System.Windows.Forms.TextBox();
            this.txt_JobLocation = new System.Windows.Forms.TextBox();
            this.txt_JobClient = new System.Windows.Forms.TextBox();
            this.txt_JobCustomer = new System.Windows.Forms.TextBox();
            this.txt_JobNumber = new System.Windows.Forms.TextBox();
            this.materialCombo = new System.Windows.Forms.ComboBox();
            this.label23 = new System.Windows.Forms.Label();
            this.SidecomboBox2 = new System.Windows.Forms.ComboBox();
            this.EndcomboBox1 = new System.Windows.Forms.ComboBox();
            this.label22 = new System.Windows.Forms.Label();
            this.comboBox1_driveDesign = new System.Windows.Forms.ComboBox();
            this.button1_save = new System.Windows.Forms.Button();
            this.label20 = new System.Windows.Forms.Label();
            this.txt_RingDepth = new System.Windows.Forms.TextBox();
            this.txt_FanDiameter = new System.Windows.Forms.TextBox();
            this.textBox_ExtraLength = new System.Windows.Forms.TextBox();
            this.txt_FanCount1 = new System.Windows.Forms.TextBox();
            this.txt_Depth1 = new System.Windows.Forms.TextBox();
            this.txt_Width1 = new System.Windows.Forms.TextBox();
            this.txt_Length1 = new System.Windows.Forms.TextBox();
            this.label18 = new System.Windows.Forms.Label();
            this.label19 = new System.Windows.Forms.Label();
            this.label_FanDiameter = new System.Windows.Forms.Label();
            this.label17 = new System.Windows.Forms.Label();
            this.checkBox1_MTRBeam = new System.Windows.Forms.CheckBox();
            this.label16 = new System.Windows.Forms.Label();
            this.comboBox_ColumnSize = new System.Windows.Forms.ComboBox();
            this.label9 = new System.Windows.Forms.Label();
            this.btn_Legacy = new System.Windows.Forms.Button();
            this.btn_Johnson = new System.Windows.Forms.Button();
            this.btn_Standard = new System.Windows.Forms.Button();
            this.label7 = new System.Windows.Forms.Label();
            this.checkBox_MidCol = new System.Windows.Forms.CheckBox();
            this.txt_FanCount = new System.Windows.Forms.Label();
            this.txt_Depth = new System.Windows.Forms.Label();
            this.txt_Width = new System.Windows.Forms.Label();
            this.txt_Length = new System.Windows.Forms.Label();
            this.txt_plen = new System.Windows.Forms.TabControl();
            this.textBox_EndOverride = new System.Windows.Forms.TextBox();
            this.textBox_DividerOverride = new System.Windows.Forms.TextBox();
            this.label36 = new System.Windows.Forms.Label();
            this.label37 = new System.Windows.Forms.Label();
            this.tab_AdvOptions.SuspendLayout();
            this.tab_Plenum.SuspendLayout();
            this.txt_plen.SuspendLayout();
            this.SuspendLayout();
            // 
            // tab_AdvOptions
            // 
            this.tab_AdvOptions.Controls.Add(this.label37);
            this.tab_AdvOptions.Controls.Add(this.label36);
            this.tab_AdvOptions.Controls.Add(this.textBox_DividerOverride);
            this.tab_AdvOptions.Controls.Add(this.textBox_EndOverride);
            this.tab_AdvOptions.Controls.Add(this.label35);
            this.tab_AdvOptions.Controls.Add(this.textBox_SpliceOverride);
            this.tab_AdvOptions.Controls.Add(this.button1);
            this.tab_AdvOptions.Controls.Add(this.checkBox4_delete);
            this.tab_AdvOptions.Controls.Add(this.checkBox3_save);
            this.tab_AdvOptions.Controls.Add(this.checkBox2_dwg);
            this.tab_AdvOptions.Controls.Add(this.leg_floor);
            this.tab_AdvOptions.Controls.Add(this.jhn_floor);
            this.tab_AdvOptions.Controls.Add(this.std_floor);
            this.tab_AdvOptions.Controls.Add(this.label27);
            this.tab_AdvOptions.Controls.Add(this.label30);
            this.tab_AdvOptions.Controls.Add(this.label31);
            this.tab_AdvOptions.Controls.Add(this.label29);
            this.tab_AdvOptions.Controls.Add(this.label28);
            this.tab_AdvOptions.Controls.Add(this.zShift_txt);
            this.tab_AdvOptions.Controls.Add(this.lengthAdj);
            this.tab_AdvOptions.Controls.Add(this.txt_xShift);
            this.tab_AdvOptions.Controls.Add(this.label25);
            this.tab_AdvOptions.Controls.Add(this.label26);
            this.tab_AdvOptions.Controls.Add(this.label24);
            this.tab_AdvOptions.Controls.Add(this.label15);
            this.tab_AdvOptions.Controls.Add(this.textBox_K1);
            this.tab_AdvOptions.Controls.Add(this.textBox_K);
            this.tab_AdvOptions.Controls.Add(this.textBox_FlangeTHK);
            this.tab_AdvOptions.Controls.Add(this.textBox_FlangeWidth);
            this.tab_AdvOptions.Controls.Add(this.textBox_WebTHK);
            this.tab_AdvOptions.Controls.Add(this.textBox_Depth);
            this.tab_AdvOptions.Controls.Add(this.label14);
            this.tab_AdvOptions.Controls.Add(this.label13);
            this.tab_AdvOptions.Controls.Add(this.label12);
            this.tab_AdvOptions.Controls.Add(this.label11);
            this.tab_AdvOptions.Controls.Add(this.label10);
            this.tab_AdvOptions.Controls.Add(this.label8);
            this.tab_AdvOptions.Location = new System.Drawing.Point(4, 22);
            this.tab_AdvOptions.Name = "tab_AdvOptions";
            this.tab_AdvOptions.Padding = new System.Windows.Forms.Padding(3);
            this.tab_AdvOptions.Size = new System.Drawing.Size(458, 581);
            this.tab_AdvOptions.TabIndex = 2;
            this.tab_AdvOptions.Text = "Advanced Options";
            this.tab_AdvOptions.UseVisualStyleBackColor = true;
            // 
            // label35
            // 
            this.label35.AutoSize = true;
            this.label35.Location = new System.Drawing.Point(158, 230);
            this.label35.Name = "label35";
            this.label35.Size = new System.Drawing.Size(168, 13);
            this.label35.TabIndex = 38;
            this.label35.Text = "Floor Splice Plate Length Override";
            // 
            // textBox_SpliceOverride
            // 
            this.textBox_SpliceOverride.Location = new System.Drawing.Point(332, 227);
            this.textBox_SpliceOverride.Name = "textBox_SpliceOverride";
            this.textBox_SpliceOverride.Size = new System.Drawing.Size(100, 20);
            this.textBox_SpliceOverride.TabIndex = 37;
            this.textBox_SpliceOverride.TextChanged += new System.EventHandler(this.textBox_SpliceOverride_TextChanged);
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(6, 6);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(54, 24);
            this.button1.TabIndex = 36;
            this.button1.Text = "Unlock";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Click += new System.EventHandler(this.button1_Click);
            // 
            // checkBox4_delete
            // 
            this.checkBox4_delete.AutoSize = true;
            this.checkBox4_delete.Location = new System.Drawing.Point(275, 109);
            this.checkBox4_delete.Name = "checkBox4_delete";
            this.checkBox4_delete.Size = new System.Drawing.Size(128, 17);
            this.checkBox4_delete.TabIndex = 31;
            this.checkBox4_delete.Text = "Delete Uneeded Files";
            this.checkBox4_delete.UseVisualStyleBackColor = true;
            this.checkBox4_delete.CheckedChanged += new System.EventHandler(this.checkBox4_delete_CheckedChanged);
            // 
            // checkBox3_save
            // 
            this.checkBox3_save.AutoSize = true;
            this.checkBox3_save.Location = new System.Drawing.Point(275, 86);
            this.checkBox3_save.Name = "checkBox3_save";
            this.checkBox3_save.Size = new System.Drawing.Size(76, 17);
            this.checkBox3_save.TabIndex = 30;
            this.checkBox3_save.Text = "Auto Save";
            this.checkBox3_save.UseVisualStyleBackColor = true;
            this.checkBox3_save.CheckedChanged += new System.EventHandler(this.checkBox3_save_CheckedChanged);
            // 
            // checkBox2_dwg
            // 
            this.checkBox2_dwg.AutoSize = true;
            this.checkBox2_dwg.Location = new System.Drawing.Point(275, 63);
            this.checkBox2_dwg.Name = "checkBox2_dwg";
            this.checkBox2_dwg.Size = new System.Drawing.Size(118, 17);
            this.checkBox2_dwg.TabIndex = 29;
            this.checkBox2_dwg.Text = "Create Drawing File";
            this.checkBox2_dwg.UseVisualStyleBackColor = true;
            this.checkBox2_dwg.CheckedChanged += new System.EventHandler(this.checkBox2_dwg_CheckedChanged);
            // 
            // leg_floor
            // 
            this.leg_floor.Location = new System.Drawing.Point(332, 494);
            this.leg_floor.Name = "leg_floor";
            this.leg_floor.Size = new System.Drawing.Size(75, 55);
            this.leg_floor.TabIndex = 27;
            this.leg_floor.Text = "Update Legacy Stiffener";
            this.leg_floor.UseVisualStyleBackColor = true;
            this.leg_floor.Click += new System.EventHandler(this.leg_floor_Click);
            // 
            // jhn_floor
            // 
            this.jhn_floor.Location = new System.Drawing.Point(185, 494);
            this.jhn_floor.Name = "jhn_floor";
            this.jhn_floor.Size = new System.Drawing.Size(75, 55);
            this.jhn_floor.TabIndex = 26;
            this.jhn_floor.Text = "Update Johnson Stiffener";
            this.jhn_floor.UseVisualStyleBackColor = true;
            this.jhn_floor.Click += new System.EventHandler(this.jhn_floor_Click);
            // 
            // std_floor
            // 
            this.std_floor.Location = new System.Drawing.Point(42, 494);
            this.std_floor.Name = "std_floor";
            this.std_floor.Size = new System.Drawing.Size(75, 55);
            this.std_floor.TabIndex = 25;
            this.std_floor.Text = "Update Standard Stiffener";
            this.std_floor.UseVisualStyleBackColor = true;
            this.std_floor.Click += new System.EventHandler(this.std_floor_Click);
            // 
            // label27
            // 
            this.label27.AutoSize = true;
            this.label27.Location = new System.Drawing.Point(139, 416);
            this.label27.Name = "label27";
            this.label27.Size = new System.Drawing.Size(61, 13);
            this.label27.TabIndex = 24;
            this.label27.Text = "End Panels";
            this.label27.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // label30
            // 
            this.label30.AutoSize = true;
            this.label30.Location = new System.Drawing.Point(46, 421);
            this.label30.Name = "label30";
            this.label30.Size = new System.Drawing.Size(95, 13);
            this.label30.TabIndex = 23;
            this.label30.Text = "[—] Shift Away from";
            this.label30.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // label31
            // 
            this.label31.AutoSize = true;
            this.label31.Location = new System.Drawing.Point(54, 408);
            this.label31.Name = "label31";
            this.label31.Size = new System.Drawing.Size(87, 13);
            this.label31.TabIndex = 22;
            this.label31.Text = "[+] Shift Towards";
            this.label31.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // label29
            // 
            this.label29.AutoSize = true;
            this.label29.Location = new System.Drawing.Point(139, 373);
            this.label29.Name = "label29";
            this.label29.Size = new System.Drawing.Size(63, 13);
            this.label29.TabIndex = 21;
            this.label29.Text = "Side Panels";
            this.label29.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // label28
            // 
            this.label28.AutoSize = true;
            this.label28.Location = new System.Drawing.Point(46, 378);
            this.label28.Name = "label28";
            this.label28.Size = new System.Drawing.Size(95, 13);
            this.label28.TabIndex = 20;
            this.label28.Text = "[—] Shift Away from";
            this.label28.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // zShift_txt
            // 
            this.zShift_txt.Location = new System.Drawing.Point(208, 413);
            this.zShift_txt.Name = "zShift_txt";
            this.zShift_txt.Size = new System.Drawing.Size(100, 20);
            this.zShift_txt.TabIndex = 19;
            this.zShift_txt.TextChanged += new System.EventHandler(this.zShift_txt_TextChanged);
            // 
            // lengthAdj
            // 
            this.lengthAdj.Location = new System.Drawing.Point(208, 459);
            this.lengthAdj.Name = "lengthAdj";
            this.lengthAdj.Size = new System.Drawing.Size(100, 20);
            this.lengthAdj.TabIndex = 17;
            this.lengthAdj.TextChanged += new System.EventHandler(this.lengthAdj_TextChanged);
            // 
            // txt_xShift
            // 
            this.txt_xShift.Location = new System.Drawing.Point(208, 370);
            this.txt_xShift.Name = "txt_xShift";
            this.txt_xShift.Size = new System.Drawing.Size(100, 20);
            this.txt_xShift.TabIndex = 15;
            this.txt_xShift.TextChanged += new System.EventHandler(this.xShift_TextChanged);
            // 
            // label25
            // 
            this.label25.AutoSize = true;
            this.label25.Location = new System.Drawing.Point(128, 462);
            this.label25.Name = "label25";
            this.label25.Size = new System.Drawing.Size(72, 13);
            this.label25.TabIndex = 16;
            this.label25.Text = "Adjust Length";
            this.label25.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // label26
            // 
            this.label26.AutoSize = true;
            this.label26.Location = new System.Drawing.Point(54, 365);
            this.label26.Name = "label26";
            this.label26.Size = new System.Drawing.Size(87, 13);
            this.label26.TabIndex = 14;
            this.label26.Text = "[+] Shift Towards";
            this.label26.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // label24
            // 
            this.label24.AutoSize = true;
            this.label24.Location = new System.Drawing.Point(194, 345);
            this.label24.Name = "label24";
            this.label24.Size = new System.Drawing.Size(127, 13);
            this.label24.TabIndex = 13;
            this.label24.Text = "Floor Stiffener Adjustment";
            // 
            // label15
            // 
            this.label15.AutoSize = true;
            this.label15.Location = new System.Drawing.Point(101, 26);
            this.label15.Name = "label15";
            this.label15.Size = new System.Drawing.Size(113, 13);
            this.label15.TabIndex = 12;
            this.label15.Text = "Column Size Overrides";
            // 
            // textBox_K1
            // 
            this.textBox_K1.Location = new System.Drawing.Point(108, 180);
            this.textBox_K1.Name = "textBox_K1";
            this.textBox_K1.Size = new System.Drawing.Size(100, 20);
            this.textBox_K1.TabIndex = 11;
            this.textBox_K1.TextChanged += new System.EventHandler(this.textBox_K1_TextChanged);
            // 
            // textBox_K
            // 
            this.textBox_K.Location = new System.Drawing.Point(108, 154);
            this.textBox_K.Name = "textBox_K";
            this.textBox_K.Size = new System.Drawing.Size(100, 20);
            this.textBox_K.TabIndex = 9;
            this.textBox_K.TextChanged += new System.EventHandler(this.textBox_K_TextChanged);
            // 
            // textBox_FlangeTHK
            // 
            this.textBox_FlangeTHK.Location = new System.Drawing.Point(108, 128);
            this.textBox_FlangeTHK.Name = "textBox_FlangeTHK";
            this.textBox_FlangeTHK.Size = new System.Drawing.Size(100, 20);
            this.textBox_FlangeTHK.TabIndex = 7;
            this.textBox_FlangeTHK.TextChanged += new System.EventHandler(this.textBox_FlangeTHK_TextChanged);
            // 
            // textBox_FlangeWidth
            // 
            this.textBox_FlangeWidth.Location = new System.Drawing.Point(108, 102);
            this.textBox_FlangeWidth.Name = "textBox_FlangeWidth";
            this.textBox_FlangeWidth.Size = new System.Drawing.Size(100, 20);
            this.textBox_FlangeWidth.TabIndex = 5;
            this.textBox_FlangeWidth.TextChanged += new System.EventHandler(this.textBox_FlangeWidth_TextChanged);
            // 
            // textBox_WebTHK
            // 
            this.textBox_WebTHK.Location = new System.Drawing.Point(108, 76);
            this.textBox_WebTHK.Name = "textBox_WebTHK";
            this.textBox_WebTHK.Size = new System.Drawing.Size(100, 20);
            this.textBox_WebTHK.TabIndex = 3;
            this.textBox_WebTHK.TextChanged += new System.EventHandler(this.textBox_WebTHK_TextChanged);
            // 
            // textBox_Depth
            // 
            this.textBox_Depth.Location = new System.Drawing.Point(108, 50);
            this.textBox_Depth.Name = "textBox_Depth";
            this.textBox_Depth.Size = new System.Drawing.Size(100, 20);
            this.textBox_Depth.TabIndex = 1;
            this.textBox_Depth.TextChanged += new System.EventHandler(this.textBox_Depth_TextChanged);
            // 
            // label14
            // 
            this.label14.AutoSize = true;
            this.label14.Location = new System.Drawing.Point(74, 183);
            this.label14.Name = "label14";
            this.label14.Size = new System.Drawing.Size(20, 13);
            this.label14.TabIndex = 10;
            this.label14.Text = "K1";
            // 
            // label13
            // 
            this.label13.AutoSize = true;
            this.label13.Location = new System.Drawing.Point(80, 157);
            this.label13.Name = "label13";
            this.label13.Size = new System.Drawing.Size(14, 13);
            this.label13.TabIndex = 8;
            this.label13.Text = "K";
            // 
            // label12
            // 
            this.label12.AutoSize = true;
            this.label12.Location = new System.Drawing.Point(33, 131);
            this.label12.Name = "label12";
            this.label12.Size = new System.Drawing.Size(61, 13);
            this.label12.TabIndex = 6;
            this.label12.Text = "FlangeTHK";
            // 
            // label11
            // 
            this.label11.AutoSize = true;
            this.label11.Location = new System.Drawing.Point(24, 105);
            this.label11.Name = "label11";
            this.label11.Size = new System.Drawing.Size(70, 13);
            this.label11.TabIndex = 4;
            this.label11.Text = "Flange Width";
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(42, 79);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(52, 13);
            this.label10.TabIndex = 2;
            this.label10.Text = "WebTHK";
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(58, 53);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(36, 13);
            this.label8.TabIndex = 0;
            this.label8.Text = "Depth";
            // 
            // tab_Plenum
            // 
            this.tab_Plenum.Controls.Add(this.label34);
            this.tab_Plenum.Controls.Add(this.flangeGageWT_Box);
            this.tab_Plenum.Controls.Add(this.label33);
            this.tab_Plenum.Controls.Add(this.braceHoleDiameter_Box);
            this.tab_Plenum.Controls.Add(this.label32);
            this.tab_Plenum.Controls.Add(this.clipTHK_Box);
            this.tab_Plenum.Controls.Add(this.label21);
            this.tab_Plenum.Controls.Add(this.mmHeight_Box);
            this.tab_Plenum.Controls.Add(this.textBox_TotalColumnHeight);
            this.tab_Plenum.Controls.Add(this.label6);
            this.tab_Plenum.Controls.Add(this.label50);
            this.tab_Plenum.Controls.Add(this.textBox_ClipHeight);
            this.tab_Plenum.Controls.Add(this.label5);
            this.tab_Plenum.Controls.Add(this.comboBox_BraceType);
            this.tab_Plenum.Controls.Add(this.textBoxShipBeamHeight);
            this.tab_Plenum.Controls.Add(this.label4);
            this.tab_Plenum.Controls.Add(this.textBox_DriveWidth);
            this.tab_Plenum.Controls.Add(this.label3);
            this.tab_Plenum.Controls.Add(this.textBox_BraceAngle);
            this.tab_Plenum.Controls.Add(this.label2);
            this.tab_Plenum.Controls.Add(this.label1);
            this.tab_Plenum.Controls.Add(this.textBox_Bank);
            this.tab_Plenum.Controls.Add(this.txt_Initials);
            this.tab_Plenum.Controls.Add(this.txt_JobItemNo);
            this.tab_Plenum.Controls.Add(this.txt_JobPO);
            this.tab_Plenum.Controls.Add(this.txt_JobLocation);
            this.tab_Plenum.Controls.Add(this.txt_JobClient);
            this.tab_Plenum.Controls.Add(this.txt_JobCustomer);
            this.tab_Plenum.Controls.Add(this.txt_JobNumber);
            this.tab_Plenum.Controls.Add(this.materialCombo);
            this.tab_Plenum.Controls.Add(this.label23);
            this.tab_Plenum.Controls.Add(this.SidecomboBox2);
            this.tab_Plenum.Controls.Add(this.EndcomboBox1);
            this.tab_Plenum.Controls.Add(this.label22);
            this.tab_Plenum.Controls.Add(this.comboBox1_driveDesign);
            this.tab_Plenum.Controls.Add(this.button1_save);
            this.tab_Plenum.Controls.Add(this.label20);
            this.tab_Plenum.Controls.Add(this.txt_RingDepth);
            this.tab_Plenum.Controls.Add(this.txt_FanDiameter);
            this.tab_Plenum.Controls.Add(this.textBox_ExtraLength);
            this.tab_Plenum.Controls.Add(this.txt_FanCount1);
            this.tab_Plenum.Controls.Add(this.txt_Depth1);
            this.tab_Plenum.Controls.Add(this.txt_Width1);
            this.tab_Plenum.Controls.Add(this.txt_Length1);
            this.tab_Plenum.Controls.Add(this.label18);
            this.tab_Plenum.Controls.Add(this.label19);
            this.tab_Plenum.Controls.Add(this.label_FanDiameter);
            this.tab_Plenum.Controls.Add(this.label17);
            this.tab_Plenum.Controls.Add(this.checkBox1_MTRBeam);
            this.tab_Plenum.Controls.Add(this.label16);
            this.tab_Plenum.Controls.Add(this.comboBox_ColumnSize);
            this.tab_Plenum.Controls.Add(this.label9);
            this.tab_Plenum.Controls.Add(this.btn_Legacy);
            this.tab_Plenum.Controls.Add(this.btn_Johnson);
            this.tab_Plenum.Controls.Add(this.btn_Standard);
            this.tab_Plenum.Controls.Add(this.label7);
            this.tab_Plenum.Controls.Add(this.checkBox_MidCol);
            this.tab_Plenum.Controls.Add(this.txt_FanCount);
            this.tab_Plenum.Controls.Add(this.txt_Depth);
            this.tab_Plenum.Controls.Add(this.txt_Width);
            this.tab_Plenum.Controls.Add(this.txt_Length);
            this.tab_Plenum.Location = new System.Drawing.Point(4, 22);
            this.tab_Plenum.Name = "tab_Plenum";
            this.tab_Plenum.Padding = new System.Windows.Forms.Padding(3);
            this.tab_Plenum.Size = new System.Drawing.Size(458, 581);
            this.tab_Plenum.TabIndex = 1;
            this.tab_Plenum.Text = "Plenum";
            this.tab_Plenum.UseVisualStyleBackColor = true;
            // 
            // label34
            // 
            this.label34.AutoSize = true;
            this.label34.Location = new System.Drawing.Point(18, 422);
            this.label34.Name = "label34";
            this.label34.Size = new System.Drawing.Size(147, 13);
            this.label34.TabIndex = 121;
            this.label34.Text = "Flange Gage (WT brace only)";
            // 
            // flangeGageWT_Box
            // 
            this.flangeGageWT_Box.Location = new System.Drawing.Point(171, 419);
            this.flangeGageWT_Box.Name = "flangeGageWT_Box";
            this.flangeGageWT_Box.Size = new System.Drawing.Size(100, 20);
            this.flangeGageWT_Box.TabIndex = 120;
            this.flangeGageWT_Box.TextChanged += new System.EventHandler(this.flangeGageWT_Box_TextChanged);
            // 
            // label33
            // 
            this.label33.AutoSize = true;
            this.label33.Location = new System.Drawing.Point(60, 396);
            this.label33.Name = "label33";
            this.label33.Size = new System.Drawing.Size(105, 13);
            this.label33.TabIndex = 119;
            this.label33.Text = "Brace Hole Diameter";
            // 
            // braceHoleDiameter_Box
            // 
            this.braceHoleDiameter_Box.Location = new System.Drawing.Point(171, 393);
            this.braceHoleDiameter_Box.Name = "braceHoleDiameter_Box";
            this.braceHoleDiameter_Box.Size = new System.Drawing.Size(100, 20);
            this.braceHoleDiameter_Box.TabIndex = 118;
            this.braceHoleDiameter_Box.TextChanged += new System.EventHandler(this.braceHoleDiameter_Box_TextChanged);
            // 
            // label32
            // 
            this.label32.AutoSize = true;
            this.label32.Location = new System.Drawing.Point(58, 370);
            this.label32.Name = "label32";
            this.label32.Size = new System.Drawing.Size(107, 13);
            this.label32.TabIndex = 117;
            this.label32.Text = "Brace Clip Thickness";
            // 
            // clipTHK_Box
            // 
            this.clipTHK_Box.Location = new System.Drawing.Point(171, 367);
            this.clipTHK_Box.Name = "clipTHK_Box";
            this.clipTHK_Box.Size = new System.Drawing.Size(100, 20);
            this.clipTHK_Box.TabIndex = 116;
            this.clipTHK_Box.TextChanged += new System.EventHandler(this.clipTHK_Box_TextChanged);
            // 
            // label21
            // 
            this.label21.AutoSize = true;
            this.label21.Location = new System.Drawing.Point(53, 250);
            this.label21.Name = "label21";
            this.label21.Size = new System.Drawing.Size(66, 13);
            this.label21.TabIndex = 115;
            this.label21.Text = "Drive Height";
            // 
            // mmHeight_Box
            // 
            this.mmHeight_Box.Location = new System.Drawing.Point(125, 247);
            this.mmHeight_Box.Name = "mmHeight_Box";
            this.mmHeight_Box.Size = new System.Drawing.Size(100, 20);
            this.mmHeight_Box.TabIndex = 114;
            this.mmHeight_Box.TextChanged += new System.EventHandler(this.mmHeight_Box_TextChanged);
            // 
            // textBox_TotalColumnHeight
            // 
            this.textBox_TotalColumnHeight.Location = new System.Drawing.Point(341, 64);
            this.textBox_TotalColumnHeight.Name = "textBox_TotalColumnHeight";
            this.textBox_TotalColumnHeight.Size = new System.Drawing.Size(100, 20);
            this.textBox_TotalColumnHeight.TabIndex = 112;
            this.textBox_TotalColumnHeight.TextChanged += new System.EventHandler(this.textBox_TotalColumnHeight_TextChanged);
            // 
            // label6
            // 
            this.label6.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(232, 67);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(103, 13);
            this.label6.TabIndex = 113;
            this.label6.Text = "Total Column Height";
            // 
            // label50
            // 
            this.label50.AutoSize = true;
            this.label50.Location = new System.Drawing.Point(76, 344);
            this.label50.Name = "label50";
            this.label50.Size = new System.Drawing.Size(89, 13);
            this.label50.TabIndex = 111;
            this.label50.Text = "Brace Clip Height";
            // 
            // textBox_ClipHeight
            // 
            this.textBox_ClipHeight.Location = new System.Drawing.Point(171, 341);
            this.textBox_ClipHeight.Name = "textBox_ClipHeight";
            this.textBox_ClipHeight.Size = new System.Drawing.Size(100, 20);
            this.textBox_ClipHeight.TabIndex = 110;
            this.textBox_ClipHeight.TextChanged += new System.EventHandler(this.textBox_ClipHeight_TextChanged);
            // 
            // label5
            // 
            this.label5.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(305, 214);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(32, 13);
            this.label5.TabIndex = 61;
            this.label5.Text = "Bank";
            // 
            // comboBox_BraceType
            // 
            this.comboBox_BraceType.FormattingEnabled = true;
            this.comboBox_BraceType.Items.AddRange(new object[] {
            "L",
            "LL",
            "T",
            "X",
            "TX"});
            this.comboBox_BraceType.Location = new System.Drawing.Point(171, 288);
            this.comboBox_BraceType.Name = "comboBox_BraceType";
            this.comboBox_BraceType.Size = new System.Drawing.Size(110, 21);
            this.comboBox_BraceType.TabIndex = 60;
            this.comboBox_BraceType.SelectedIndexChanged += new System.EventHandler(this.comboBox_BraceType_SelectedIndexChanged);
            // 
            // textBoxShipBeamHeight
            // 
            this.textBoxShipBeamHeight.Location = new System.Drawing.Point(113, 465);
            this.textBoxShipBeamHeight.Name = "textBoxShipBeamHeight";
            this.textBoxShipBeamHeight.Size = new System.Drawing.Size(100, 20);
            this.textBoxShipBeamHeight.TabIndex = 58;
            this.textBoxShipBeamHeight.TextChanged += new System.EventHandler(this.textBoxShipBeamHeight_TextChanged);
            // 
            // label4
            // 
            this.label4.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(15, 468);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(92, 13);
            this.label4.TabIndex = 59;
            this.label4.Text = "Ship Beam Height";
            // 
            // textBox_DriveWidth
            // 
            this.textBox_DriveWidth.Location = new System.Drawing.Point(125, 219);
            this.textBox_DriveWidth.Name = "textBox_DriveWidth";
            this.textBox_DriveWidth.Size = new System.Drawing.Size(100, 20);
            this.textBox_DriveWidth.TabIndex = 56;
            this.textBox_DriveWidth.TextChanged += new System.EventHandler(this.textBox_DriveWidth_TextChanged);
            // 
            // label3
            // 
            this.label3.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(53, 222);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(63, 13);
            this.label3.TabIndex = 57;
            this.label3.Text = "Drive Width";
            this.label3.Click += new System.EventHandler(this.label3_Click);
            // 
            // textBox_BraceAngle
            // 
            this.textBox_BraceAngle.Location = new System.Drawing.Point(171, 315);
            this.textBox_BraceAngle.Name = "textBox_BraceAngle";
            this.textBox_BraceAngle.Size = new System.Drawing.Size(100, 20);
            this.textBox_BraceAngle.TabIndex = 54;
            this.textBox_BraceAngle.TextChanged += new System.EventHandler(this.textBox_BraceAngle_TextChanged);
            // 
            // label2
            // 
            this.label2.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(102, 318);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(65, 13);
            this.label2.TabIndex = 55;
            this.label2.Text = "Brace Angle";
            // 
            // label1
            // 
            this.label1.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(103, 291);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(62, 13);
            this.label1.TabIndex = 53;
            this.label1.Text = "Brace Type";
            // 
            // textBox_Bank
            // 
            this.textBox_Bank.Location = new System.Drawing.Point(341, 211);
            this.textBox_Bank.Name = "textBox_Bank";
            this.textBox_Bank.Size = new System.Drawing.Size(20, 20);
            this.textBox_Bank.TabIndex = 51;
            this.textBox_Bank.TextChanged += new System.EventHandler(this.textBox_Bank_TextChanged);
            // 
            // txt_Initials
            // 
            this.txt_Initials.Location = new System.Drawing.Point(304, 396);
            this.txt_Initials.Name = "txt_Initials";
            this.txt_Initials.Size = new System.Drawing.Size(100, 20);
            this.txt_Initials.TabIndex = 50;
            this.txt_Initials.TextChanged += new System.EventHandler(this.txt_Initials_TextChanged_1);
            // 
            // txt_JobItemNo
            // 
            this.txt_JobItemNo.Location = new System.Drawing.Point(304, 367);
            this.txt_JobItemNo.Name = "txt_JobItemNo";
            this.txt_JobItemNo.Size = new System.Drawing.Size(100, 20);
            this.txt_JobItemNo.TabIndex = 49;
            this.txt_JobItemNo.TextChanged += new System.EventHandler(this.txt_JobItemNo_TextChanged_1);
            // 
            // txt_JobPO
            // 
            this.txt_JobPO.Location = new System.Drawing.Point(304, 341);
            this.txt_JobPO.Name = "txt_JobPO";
            this.txt_JobPO.Size = new System.Drawing.Size(100, 20);
            this.txt_JobPO.TabIndex = 48;
            this.txt_JobPO.TextChanged += new System.EventHandler(this.txt_JobPO_TextChanged_1);
            // 
            // txt_JobLocation
            // 
            this.txt_JobLocation.Location = new System.Drawing.Point(304, 315);
            this.txt_JobLocation.Name = "txt_JobLocation";
            this.txt_JobLocation.Size = new System.Drawing.Size(100, 20);
            this.txt_JobLocation.TabIndex = 47;
            this.txt_JobLocation.TextChanged += new System.EventHandler(this.txt_JobLocation_TextChanged_1);
            // 
            // txt_JobClient
            // 
            this.txt_JobClient.Location = new System.Drawing.Point(304, 289);
            this.txt_JobClient.Name = "txt_JobClient";
            this.txt_JobClient.Size = new System.Drawing.Size(100, 20);
            this.txt_JobClient.TabIndex = 46;
            this.txt_JobClient.TextChanged += new System.EventHandler(this.txt_JobClient_TextChanged_1);
            // 
            // txt_JobCustomer
            // 
            this.txt_JobCustomer.Location = new System.Drawing.Point(304, 263);
            this.txt_JobCustomer.Name = "txt_JobCustomer";
            this.txt_JobCustomer.Size = new System.Drawing.Size(100, 20);
            this.txt_JobCustomer.TabIndex = 45;
            this.txt_JobCustomer.TextChanged += new System.EventHandler(this.txt_JobCustomer_TextChanged);
            // 
            // txt_JobNumber
            // 
            this.txt_JobNumber.Location = new System.Drawing.Point(304, 237);
            this.txt_JobNumber.Name = "txt_JobNumber";
            this.txt_JobNumber.Size = new System.Drawing.Size(100, 20);
            this.txt_JobNumber.TabIndex = 44;
            this.txt_JobNumber.TextChanged += new System.EventHandler(this.txt_JobNumber_TextChanged);
            // 
            // materialCombo
            // 
            this.materialCombo.FormattingEnabled = true;
            this.materialCombo.Items.AddRange(new object[] {
            "A36",
            "A572_50"});
            this.materialCombo.Location = new System.Drawing.Point(331, 453);
            this.materialCombo.Name = "materialCombo";
            this.materialCombo.Size = new System.Drawing.Size(110, 21);
            this.materialCombo.TabIndex = 43;
            this.materialCombo.SelectedIndexChanged += new System.EventHandler(this.materialCombo_SelectedIndexChanged);
            // 
            // label23
            // 
            this.label23.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.label23.AutoSize = true;
            this.label23.Location = new System.Drawing.Point(280, 456);
            this.label23.Name = "label23";
            this.label23.Size = new System.Drawing.Size(44, 13);
            this.label23.TabIndex = 42;
            this.label23.Text = "Material";
            this.label23.Click += new System.EventHandler(this.label23_Click);
            // 
            // SidecomboBox2
            // 
            this.SidecomboBox2.FormattingEnabled = true;
            this.SidecomboBox2.Items.AddRange(new object[] {
            "0.1344",
            "0.1875",
            "0.2500",
            "0.3125",
            "0.3750"});
            this.SidecomboBox2.Location = new System.Drawing.Point(341, 156);
            this.SidecomboBox2.Name = "SidecomboBox2";
            this.SidecomboBox2.Size = new System.Drawing.Size(100, 21);
            this.SidecomboBox2.TabIndex = 41;
            this.SidecomboBox2.SelectedIndexChanged += new System.EventHandler(this.sideComboBox2_SelectedIndexChanged);
            // 
            // EndcomboBox1
            // 
            this.EndcomboBox1.FormattingEnabled = true;
            this.EndcomboBox1.Items.AddRange(new object[] {
            "0.1344",
            "0.1875",
            "0.2500",
            "0.3125",
            "0.3750"});
            this.EndcomboBox1.Location = new System.Drawing.Point(341, 130);
            this.EndcomboBox1.Name = "EndcomboBox1";
            this.EndcomboBox1.Size = new System.Drawing.Size(100, 21);
            this.EndcomboBox1.TabIndex = 40;
            this.EndcomboBox1.SelectedIndexChanged += new System.EventHandler(this.endComboBox1_SelectedIndexChanged);
            // 
            // label22
            // 
            this.label22.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.label22.AutoSize = true;
            this.label22.Location = new System.Drawing.Point(51, 195);
            this.label22.Name = "label22";
            this.label22.Size = new System.Drawing.Size(68, 13);
            this.label22.TabIndex = 39;
            this.label22.Text = "Drive Design";
            // 
            // comboBox1_driveDesign
            // 
            this.comboBox1_driveDesign.FormattingEnabled = true;
            this.comboBox1_driveDesign.Items.AddRange(new object[] {
            "Motor Shaft Down",
            "Motor Shaft Up"});
            this.comboBox1_driveDesign.Location = new System.Drawing.Point(125, 192);
            this.comboBox1_driveDesign.Name = "comboBox1_driveDesign";
            this.comboBox1_driveDesign.Size = new System.Drawing.Size(110, 21);
            this.comboBox1_driveDesign.TabIndex = 38;
            this.comboBox1_driveDesign.SelectedIndexChanged += new System.EventHandler(this.comboBox1_driveDesign_SelectedIndexChanged);
            // 
            // button1_save
            // 
            this.button1_save.Location = new System.Drawing.Point(3, 4);
            this.button1_save.Name = "button1_save";
            this.button1_save.Size = new System.Drawing.Size(40, 24);
            this.button1_save.TabIndex = 35;
            this.button1_save.Text = "Save";
            this.button1_save.UseVisualStyleBackColor = true;
            this.button1_save.Click += new System.EventHandler(this.button1_save_Click);
            // 
            // label20
            // 
            this.label20.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.label20.AutoSize = true;
            this.label20.Location = new System.Drawing.Point(11, 154);
            this.label20.Name = "label20";
            this.label20.Size = new System.Drawing.Size(82, 13);
            this.label20.TabIndex = 34;
            this.label20.Text = "Fan Ring Depth";
            // 
            // txt_RingDepth
            // 
            this.txt_RingDepth.Location = new System.Drawing.Point(99, 151);
            this.txt_RingDepth.Name = "txt_RingDepth";
            this.txt_RingDepth.Size = new System.Drawing.Size(100, 20);
            this.txt_RingDepth.TabIndex = 33;
            this.txt_RingDepth.TextChanged += new System.EventHandler(this.txt_RingDepth_TextChanged);
            // 
            // txt_FanDiameter
            // 
            this.txt_FanDiameter.Location = new System.Drawing.Point(99, 125);
            this.txt_FanDiameter.Name = "txt_FanDiameter";
            this.txt_FanDiameter.Size = new System.Drawing.Size(100, 20);
            this.txt_FanDiameter.TabIndex = 27;
            this.txt_FanDiameter.TextChanged += new System.EventHandler(this.txt_FanDiameter_TextChanged);
            // 
            // textBox_ExtraLength
            // 
            this.textBox_ExtraLength.Location = new System.Drawing.Point(186, 505);
            this.textBox_ExtraLength.Name = "textBox_ExtraLength";
            this.textBox_ExtraLength.Size = new System.Drawing.Size(75, 20);
            this.textBox_ExtraLength.TabIndex = 23;
            this.textBox_ExtraLength.TextChanged += new System.EventHandler(this.textBox_ExtraLength_TextChanged);
            // 
            // txt_FanCount1
            // 
            this.txt_FanCount1.Location = new System.Drawing.Point(99, 99);
            this.txt_FanCount1.Name = "txt_FanCount1";
            this.txt_FanCount1.Size = new System.Drawing.Size(100, 20);
            this.txt_FanCount1.TabIndex = 13;
            this.txt_FanCount1.TextChanged += new System.EventHandler(this.txt_FanCount1_TextChanged);
            // 
            // txt_Depth1
            // 
            this.txt_Depth1.Location = new System.Drawing.Point(99, 64);
            this.txt_Depth1.Name = "txt_Depth1";
            this.txt_Depth1.Size = new System.Drawing.Size(100, 20);
            this.txt_Depth1.TabIndex = 11;
            this.txt_Depth1.TextChanged += new System.EventHandler(this.txt_Depth1_TextChanged);
            // 
            // txt_Width1
            // 
            this.txt_Width1.Location = new System.Drawing.Point(99, 12);
            this.txt_Width1.Name = "txt_Width1";
            this.txt_Width1.Size = new System.Drawing.Size(100, 20);
            this.txt_Width1.TabIndex = 9;
            this.txt_Width1.TextChanged += new System.EventHandler(this.txt_Width1_TextChanged);
            // 
            // txt_Length1
            // 
            this.txt_Length1.Location = new System.Drawing.Point(99, 38);
            this.txt_Length1.Name = "txt_Length1";
            this.txt_Length1.Size = new System.Drawing.Size(100, 20);
            this.txt_Length1.TabIndex = 7;
            this.txt_Length1.TextChanged += new System.EventHandler(this.txt_Length_TextChanged);
            // 
            // label18
            // 
            this.label18.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.label18.AutoSize = true;
            this.label18.Location = new System.Drawing.Point(227, 159);
            this.label18.Name = "label18";
            this.label18.Size = new System.Drawing.Size(110, 13);
            this.label18.TabIndex = 32;
            this.label18.Text = "Side Panel Thickness";
            // 
            // label19
            // 
            this.label19.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.label19.AutoSize = true;
            this.label19.Location = new System.Drawing.Point(227, 133);
            this.label19.Name = "label19";
            this.label19.Size = new System.Drawing.Size(108, 13);
            this.label19.TabIndex = 30;
            this.label19.Text = "End Panel Thickness";
            // 
            // label_FanDiameter
            // 
            this.label_FanDiameter.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.label_FanDiameter.AutoSize = true;
            this.label_FanDiameter.Location = new System.Drawing.Point(26, 128);
            this.label_FanDiameter.Name = "label_FanDiameter";
            this.label_FanDiameter.Size = new System.Drawing.Size(70, 13);
            this.label_FanDiameter.TabIndex = 28;
            this.label_FanDiameter.Text = "Fan Diameter";
            // 
            // label17
            // 
            this.label17.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.label17.AutoSize = true;
            this.label17.Location = new System.Drawing.Point(225, 99);
            this.label17.Name = "label17";
            this.label17.Size = new System.Drawing.Size(109, 13);
            this.label17.TabIndex = 26;
            this.label17.Text = "Motor Removal Beam";
            // 
            // checkBox1_MTRBeam
            // 
            this.checkBox1_MTRBeam.AutoSize = true;
            this.checkBox1_MTRBeam.Location = new System.Drawing.Point(341, 99);
            this.checkBox1_MTRBeam.Name = "checkBox1_MTRBeam";
            this.checkBox1_MTRBeam.Size = new System.Drawing.Size(15, 14);
            this.checkBox1_MTRBeam.TabIndex = 25;
            this.checkBox1_MTRBeam.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.checkBox1_MTRBeam.UseVisualStyleBackColor = true;
            this.checkBox1_MTRBeam.CheckedChanged += new System.EventHandler(this.checkBox1_MTRBeam_CheckedChanged);
            // 
            // label16
            // 
            this.label16.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.label16.AutoSize = true;
            this.label16.Location = new System.Drawing.Point(169, 524);
            this.label16.Name = "label16";
            this.label16.Size = new System.Drawing.Size(105, 13);
            this.label16.TabIndex = 24;
            this.label16.Text = "Extra Plenum Length";
            // 
            // comboBox_ColumnSize
            // 
            this.comboBox_ColumnSize.FormattingEnabled = true;
            this.comboBox_ColumnSize.Items.AddRange(new object[] {
            "W6x15",
            "W6x20",
            "W6x25",
            "W8x31"});
            this.comboBox_ColumnSize.Location = new System.Drawing.Point(341, 38);
            this.comboBox_ColumnSize.Name = "comboBox_ColumnSize";
            this.comboBox_ColumnSize.Size = new System.Drawing.Size(100, 21);
            this.comboBox_ColumnSize.TabIndex = 22;
            this.comboBox_ColumnSize.SelectedIndexChanged += new System.EventHandler(this.comboBox_ColumnSize_SelectedIndexChanged);
            // 
            // label9
            // 
            this.label9.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(272, 41);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(65, 13);
            this.label9.TabIndex = 21;
            this.label9.Text = "Column Size";
            // 
            // btn_Legacy
            // 
            this.btn_Legacy.Location = new System.Drawing.Point(333, 541);
            this.btn_Legacy.Name = "btn_Legacy";
            this.btn_Legacy.Size = new System.Drawing.Size(75, 23);
            this.btn_Legacy.TabIndex = 19;
            this.btn_Legacy.Text = "Legacy";
            this.btn_Legacy.UseVisualStyleBackColor = true;
            this.btn_Legacy.Click += new System.EventHandler(this.btn_Legacy_Click);
            // 
            // btn_Johnson
            // 
            this.btn_Johnson.Location = new System.Drawing.Point(186, 541);
            this.btn_Johnson.Name = "btn_Johnson";
            this.btn_Johnson.Size = new System.Drawing.Size(75, 23);
            this.btn_Johnson.TabIndex = 18;
            this.btn_Johnson.Text = "Johnson";
            this.btn_Johnson.UseVisualStyleBackColor = true;
            this.btn_Johnson.Click += new System.EventHandler(this.btn_Johnson_Click);
            // 
            // btn_Standard
            // 
            this.btn_Standard.Location = new System.Drawing.Point(43, 541);
            this.btn_Standard.Name = "btn_Standard";
            this.btn_Standard.Size = new System.Drawing.Size(75, 23);
            this.btn_Standard.TabIndex = 17;
            this.btn_Standard.Text = "Standard";
            this.btn_Standard.UseVisualStyleBackColor = true;
            this.btn_Standard.Click += new System.EventHandler(this.btn_Standard_Click);
            // 
            // label7
            // 
            this.label7.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(270, 19);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(67, 13);
            this.label7.TabIndex = 16;
            this.label7.Text = "Mid Columns";
            // 
            // checkBox_MidCol
            // 
            this.checkBox_MidCol.AutoSize = true;
            this.checkBox_MidCol.Location = new System.Drawing.Point(341, 18);
            this.checkBox_MidCol.Name = "checkBox_MidCol";
            this.checkBox_MidCol.Size = new System.Drawing.Size(15, 14);
            this.checkBox_MidCol.TabIndex = 15;
            this.checkBox_MidCol.UseVisualStyleBackColor = true;
            this.checkBox_MidCol.CheckedChanged += new System.EventHandler(this.checkBox_MidCol_CheckedChanged);
            // 
            // txt_FanCount
            // 
            this.txt_FanCount.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.txt_FanCount.AutoSize = true;
            this.txt_FanCount.Location = new System.Drawing.Point(37, 102);
            this.txt_FanCount.Name = "txt_FanCount";
            this.txt_FanCount.Size = new System.Drawing.Size(56, 13);
            this.txt_FanCount.TabIndex = 14;
            this.txt_FanCount.Text = "Fan Count";
            // 
            // txt_Depth
            // 
            this.txt_Depth.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.txt_Depth.AutoSize = true;
            this.txt_Depth.Location = new System.Drawing.Point(58, 67);
            this.txt_Depth.Name = "txt_Depth";
            this.txt_Depth.Size = new System.Drawing.Size(36, 13);
            this.txt_Depth.TabIndex = 12;
            this.txt_Depth.Text = "Depth";
            // 
            // txt_Width
            // 
            this.txt_Width.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.txt_Width.AutoSize = true;
            this.txt_Width.Location = new System.Drawing.Point(58, 15);
            this.txt_Width.Name = "txt_Width";
            this.txt_Width.Size = new System.Drawing.Size(35, 13);
            this.txt_Width.TabIndex = 10;
            this.txt_Width.Text = "Width";
            // 
            // txt_Length
            // 
            this.txt_Length.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.txt_Length.AutoSize = true;
            this.txt_Length.Location = new System.Drawing.Point(53, 41);
            this.txt_Length.Name = "txt_Length";
            this.txt_Length.Size = new System.Drawing.Size(40, 13);
            this.txt_Length.TabIndex = 8;
            this.txt_Length.Text = "Length";
            // 
            // txt_plen
            // 
            this.txt_plen.Controls.Add(this.tab_Plenum);
            this.txt_plen.Controls.Add(this.tab_AdvOptions);
            this.txt_plen.Location = new System.Drawing.Point(12, 12);
            this.txt_plen.Name = "txt_plen";
            this.txt_plen.SelectedIndex = 0;
            this.txt_plen.Size = new System.Drawing.Size(466, 607);
            this.txt_plen.TabIndex = 2;
            // 
            // textBox_EndOverride
            // 
            this.textBox_EndOverride.Location = new System.Drawing.Point(57, 265);
            this.textBox_EndOverride.Name = "textBox_EndOverride";
            this.textBox_EndOverride.Size = new System.Drawing.Size(100, 20);
            this.textBox_EndOverride.TabIndex = 39;
            this.textBox_EndOverride.TextChanged += new System.EventHandler(this.textBox_EndOverride_TextChanged);
            // 
            // textBox_DividerOverride
            // 
            this.textBox_DividerOverride.Location = new System.Drawing.Point(57, 291);
            this.textBox_DividerOverride.Name = "textBox_DividerOverride";
            this.textBox_DividerOverride.Size = new System.Drawing.Size(100, 20);
            this.textBox_DividerOverride.TabIndex = 40;
            this.textBox_DividerOverride.TextChanged += new System.EventHandler(this.textBox_SideOverride_TextChanged);
            // 
            // label36
            // 
            this.label36.AutoSize = true;
            this.label36.Location = new System.Drawing.Point(163, 268);
            this.label36.Name = "label36";
            this.label36.Size = new System.Drawing.Size(168, 13);
            this.label36.TabIndex = 41;
            this.label36.Text = "Number of End Stiffeners Override";
            // 
            // label37
            // 
            this.label37.AutoSize = true;
            this.label37.Location = new System.Drawing.Point(163, 294);
            this.label37.Name = "label37";
            this.label37.Size = new System.Drawing.Size(182, 13);
            this.label37.TabIndex = 42;
            this.label37.Text = "Number of Divider Stiffeners Override";
            // 
            // PlenumUI
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(490, 629);
            this.Controls.Add(this.txt_plen);
            this.Name = "PlenumUI";
            this.Text = "PlenumUI";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.PlenumUI_FormClosing);
            this.Load += new System.EventHandler(this.PlenumUI_Load);
            this.tab_AdvOptions.ResumeLayout(false);
            this.tab_AdvOptions.PerformLayout();
            this.tab_Plenum.ResumeLayout(false);
            this.tab_Plenum.PerformLayout();
            this.txt_plen.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion
        private System.Windows.Forms.TabPage tab_AdvOptions;
        private System.Windows.Forms.Label label15;
        private System.Windows.Forms.TextBox textBox_K1;
        private System.Windows.Forms.TextBox textBox_K;
        private System.Windows.Forms.TextBox textBox_FlangeTHK;
        private System.Windows.Forms.TextBox textBox_FlangeWidth;
        private System.Windows.Forms.TextBox textBox_WebTHK;
        private System.Windows.Forms.TextBox textBox_Depth;
        private System.Windows.Forms.Label label14;
        private System.Windows.Forms.Label label13;
        private System.Windows.Forms.Label label12;
        private System.Windows.Forms.Label label11;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.TabPage tab_Plenum;
        private System.Windows.Forms.Button button1_save;
        private System.Windows.Forms.Label label20;
        private System.Windows.Forms.TextBox txt_RingDepth;
        private System.Windows.Forms.TextBox txt_FanDiameter;
        private System.Windows.Forms.TextBox textBox_ExtraLength;
        private System.Windows.Forms.TextBox txt_FanCount1;
        private System.Windows.Forms.TextBox txt_Depth1;
        private System.Windows.Forms.TextBox txt_Width1;
        private System.Windows.Forms.TextBox txt_Length1;
        private System.Windows.Forms.Label label18;
        private System.Windows.Forms.Label label19;
        private System.Windows.Forms.Label label_FanDiameter;
        private System.Windows.Forms.Label label17;
        private System.Windows.Forms.CheckBox checkBox1_MTRBeam;
        private System.Windows.Forms.Label label16;
        private System.Windows.Forms.ComboBox comboBox_ColumnSize;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.Button btn_Legacy;
        private System.Windows.Forms.Button btn_Johnson;
        private System.Windows.Forms.Button btn_Standard;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.CheckBox checkBox_MidCol;
        private System.Windows.Forms.Label txt_FanCount;
        private System.Windows.Forms.Label txt_Depth;
        private System.Windows.Forms.Label txt_Width;
        private System.Windows.Forms.Label txt_Length;
        private System.Windows.Forms.TabControl txt_plen;
        private System.Windows.Forms.Label label22;
        private System.Windows.Forms.ComboBox comboBox1_driveDesign;
        private System.Windows.Forms.ComboBox SidecomboBox2;
        private System.Windows.Forms.ComboBox EndcomboBox1;
        private System.Windows.Forms.ComboBox materialCombo;
        private System.Windows.Forms.Label label23;
        private System.Windows.Forms.Label label24;
        private System.Windows.Forms.TextBox lengthAdj;
        private System.Windows.Forms.TextBox txt_xShift;
        private System.Windows.Forms.Label label25;
        private System.Windows.Forms.Label label26;
        private System.Windows.Forms.Label label28;
        private System.Windows.Forms.TextBox zShift_txt;
        private System.Windows.Forms.Label label27;
        private System.Windows.Forms.Label label30;
        private System.Windows.Forms.Label label31;
        private System.Windows.Forms.Label label29;
        private System.Windows.Forms.Button leg_floor;
        private System.Windows.Forms.Button jhn_floor;
        private System.Windows.Forms.Button std_floor;
        private System.Windows.Forms.CheckBox checkBox4_delete;
        private System.Windows.Forms.CheckBox checkBox3_save;
        private System.Windows.Forms.CheckBox checkBox2_dwg;
        private System.Windows.Forms.Button button1;
        private System.Windows.Forms.TextBox txt_Initials;
        private System.Windows.Forms.TextBox txt_JobItemNo;
        private System.Windows.Forms.TextBox txt_JobPO;
        private System.Windows.Forms.TextBox txt_JobLocation;
        private System.Windows.Forms.TextBox txt_JobClient;
        private System.Windows.Forms.TextBox txt_JobCustomer;
        private System.Windows.Forms.TextBox txt_JobNumber;
        private System.Windows.Forms.TextBox textBox_Bank;
        private System.Windows.Forms.ComboBox comboBox_BraceType;
        private System.Windows.Forms.TextBox textBoxShipBeamHeight;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.TextBox textBox_DriveWidth;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.TextBox textBox_BraceAngle;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Label label50;
        private System.Windows.Forms.TextBox textBox_ClipHeight;
        private System.Windows.Forms.TextBox textBox_TotalColumnHeight;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.Label label21;
        private System.Windows.Forms.TextBox mmHeight_Box;
        private System.Windows.Forms.Label label32;
        private System.Windows.Forms.TextBox clipTHK_Box;
        private System.Windows.Forms.TextBox braceHoleDiameter_Box;
        private System.Windows.Forms.Label label33;
        private System.Windows.Forms.Label label34;
        private System.Windows.Forms.TextBox flangeGageWT_Box;
        private System.Windows.Forms.Label label35;
        private System.Windows.Forms.TextBox textBox_SpliceOverride;
        private System.Windows.Forms.Label label37;
        private System.Windows.Forms.Label label36;
        private System.Windows.Forms.TextBox textBox_DividerOverride;
        private System.Windows.Forms.TextBox textBox_EndOverride;
    }
}