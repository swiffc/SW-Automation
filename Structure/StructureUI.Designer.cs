using static FileTools.CommonData.CommonData;

namespace Structure
{
    partial class StructureUI
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
            this.txt_HandrailBank = new System.Windows.Forms.TabControl();
            this.tabPage2 = new System.Windows.Forms.TabPage();
            this.label53 = new System.Windows.Forms.Label();
            this.label52 = new System.Windows.Forms.Label();
            this.textBoxShipBeamHeight = new System.Windows.Forms.TextBox();
            this.textBox_DriveWidth = new System.Windows.Forms.TextBox();
            this.label48 = new System.Windows.Forms.Label();
            this.label51 = new System.Windows.Forms.Label();
            this.label49 = new System.Windows.Forms.Label();
            this.textBox_BasePlateTHK = new System.Windows.Forms.TextBox();
            this.textBox_Bank = new System.Windows.Forms.TextBox();
            this.label47 = new System.Windows.Forms.Label();
            this.materialCombo = new System.Windows.Forms.ComboBox();
            this.label45 = new System.Windows.Forms.Label();
            this.button1_save = new System.Windows.Forms.Button();
            this.beamSize_Box = new System.Windows.Forms.ComboBox();
            this.label19 = new System.Windows.Forms.Label();
            this.label17 = new System.Windows.Forms.Label();
            this.rotate_Box = new System.Windows.Forms.CheckBox();
            this.initials_Box = new System.Windows.Forms.TextBox();
            this.itemNumber_Box = new System.Windows.Forms.TextBox();
            this.purchaseOrder_Box = new System.Windows.Forms.TextBox();
            this.location_Box = new System.Windows.Forms.TextBox();
            this.client_Box = new System.Windows.Forms.TextBox();
            this.customer_Box = new System.Windows.Forms.TextBox();
            this.job_Box = new System.Windows.Forms.TextBox();
            this.label15 = new System.Windows.Forms.Label();
            this.label16 = new System.Windows.Forms.Label();
            this.mmHeight_Box = new System.Windows.Forms.TextBox();
            this.label14 = new System.Windows.Forms.Label();
            this.depth_Box = new System.Windows.Forms.TextBox();
            this.label13 = new System.Windows.Forms.Label();
            this.label12 = new System.Windows.Forms.Label();
            this.dia_Box = new System.Windows.Forms.TextBox();
            this.label11 = new System.Windows.Forms.Label();
            this.lSPA_Box = new System.Windows.Forms.TextBox();
            this.label10 = new System.Windows.Forms.Label();
            this.wSPA_Box = new System.Windows.Forms.TextBox();
            this.label6 = new System.Windows.Forms.Label();
            this.bpLength_Box = new System.Windows.Forms.TextBox();
            this.label5 = new System.Windows.Forms.Label();
            this.bpWidth_Box = new System.Windows.Forms.TextBox();
            this.label4 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.fanCount_Box = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.midColumns_Box = new System.Windows.Forms.CheckBox();
            this.label9 = new System.Windows.Forms.Label();
            this.height_TextBox = new System.Windows.Forms.TextBox();
            this.label8 = new System.Windows.Forms.Label();
            this.length_TextBox = new System.Windows.Forms.TextBox();
            this.label7 = new System.Windows.Forms.Label();
            this.width_TextBox = new System.Windows.Forms.TextBox();
            this.btn_Standard = new System.Windows.Forms.Button();
            this.tabPage3 = new System.Windows.Forms.TabPage();
            this.label50 = new System.Windows.Forms.Label();
            this.textBox_ClipHeight = new System.Windows.Forms.TextBox();
            this.button1 = new System.Windows.Forms.Button();
            this.label44 = new System.Windows.Forms.Label();
            this.label42 = new System.Windows.Forms.Label();
            this.flangeGageWT_Box = new System.Windows.Forms.TextBox();
            this.label43 = new System.Windows.Forms.Label();
            this.k1WT_Box = new System.Windows.Forms.TextBox();
            this.label37 = new System.Windows.Forms.Label();
            this.kWT_Box = new System.Windows.Forms.TextBox();
            this.label38 = new System.Windows.Forms.Label();
            this.flangeTHKWT_Box = new System.Windows.Forms.TextBox();
            this.label39 = new System.Windows.Forms.Label();
            this.flangeWidthWT_Box = new System.Windows.Forms.TextBox();
            this.label40 = new System.Windows.Forms.Label();
            this.stemTHKWT_Box = new System.Windows.Forms.TextBox();
            this.label41 = new System.Windows.Forms.Label();
            this.depthWT_Box = new System.Windows.Forms.TextBox();
            this.label36 = new System.Windows.Forms.Label();
            this.label35 = new System.Windows.Forms.Label();
            this.kL_Box = new System.Windows.Forms.TextBox();
            this.label34 = new System.Windows.Forms.Label();
            this.thkL_Box = new System.Windows.Forms.TextBox();
            this.label33 = new System.Windows.Forms.Label();
            this.gage_Box = new System.Windows.Forms.TextBox();
            this.label24 = new System.Windows.Forms.Label();
            this.leg2_Box = new System.Windows.Forms.TextBox();
            this.label23 = new System.Windows.Forms.Label();
            this.leg1_Box = new System.Windows.Forms.TextBox();
            this.label25 = new System.Windows.Forms.Label();
            this.clipTHK_Box = new System.Windows.Forms.TextBox();
            this.label22 = new System.Windows.Forms.Label();
            this.braceAngle_Box = new System.Windows.Forms.TextBox();
            this.label21 = new System.Windows.Forms.Label();
            this.braceHoleDiameter_Box = new System.Windows.Forms.TextBox();
            this.label20 = new System.Windows.Forms.Label();
            this.label18 = new System.Windows.Forms.Label();
            this.braceType_Box = new System.Windows.Forms.ComboBox();
            this.tabPage1 = new System.Windows.Forms.TabPage();
            this.delete_Toggle = new System.Windows.Forms.CheckBox();
            this.save_Toggle = new System.Windows.Forms.CheckBox();
            this.createDrawing_Toggle = new System.Windows.Forms.CheckBox();
            this.label26 = new System.Windows.Forms.Label();
            this.textBox_K1 = new System.Windows.Forms.TextBox();
            this.textBox_K = new System.Windows.Forms.TextBox();
            this.textBox_FlangeTHK = new System.Windows.Forms.TextBox();
            this.textBox_FlangeWidth = new System.Windows.Forms.TextBox();
            this.textBox_WebTHK = new System.Windows.Forms.TextBox();
            this.textBox_Depth = new System.Windows.Forms.TextBox();
            this.label27 = new System.Windows.Forms.Label();
            this.label28 = new System.Windows.Forms.Label();
            this.label29 = new System.Windows.Forms.Label();
            this.label30 = new System.Windows.Forms.Label();
            this.label31 = new System.Windows.Forms.Label();
            this.label32 = new System.Windows.Forms.Label();
            this.txt_HandrailBank.SuspendLayout();
            this.tabPage2.SuspendLayout();
            this.tabPage3.SuspendLayout();
            this.tabPage1.SuspendLayout();
            this.SuspendLayout();
            // 
            // txt_HandrailBank
            // 
            this.txt_HandrailBank.Controls.Add(this.tabPage2);
            this.txt_HandrailBank.Controls.Add(this.tabPage3);
            this.txt_HandrailBank.Controls.Add(this.tabPage1);
            this.txt_HandrailBank.Location = new System.Drawing.Point(12, 12);
            this.txt_HandrailBank.Name = "txt_HandrailBank";
            this.txt_HandrailBank.SelectedIndex = 0;
            this.txt_HandrailBank.Size = new System.Drawing.Size(425, 548);
            this.txt_HandrailBank.TabIndex = 1;
            // 
            // tabPage2
            // 
            this.tabPage2.Controls.Add(this.label53);
            this.tabPage2.Controls.Add(this.label52);
            this.tabPage2.Controls.Add(this.textBoxShipBeamHeight);
            this.tabPage2.Controls.Add(this.textBox_DriveWidth);
            this.tabPage2.Controls.Add(this.label48);
            this.tabPage2.Controls.Add(this.label51);
            this.tabPage2.Controls.Add(this.label49);
            this.tabPage2.Controls.Add(this.textBox_BasePlateTHK);
            this.tabPage2.Controls.Add(this.textBox_Bank);
            this.tabPage2.Controls.Add(this.label47);
            this.tabPage2.Controls.Add(this.materialCombo);
            this.tabPage2.Controls.Add(this.label45);
            this.tabPage2.Controls.Add(this.button1_save);
            this.tabPage2.Controls.Add(this.beamSize_Box);
            this.tabPage2.Controls.Add(this.label19);
            this.tabPage2.Controls.Add(this.label17);
            this.tabPage2.Controls.Add(this.rotate_Box);
            this.tabPage2.Controls.Add(this.initials_Box);
            this.tabPage2.Controls.Add(this.itemNumber_Box);
            this.tabPage2.Controls.Add(this.purchaseOrder_Box);
            this.tabPage2.Controls.Add(this.location_Box);
            this.tabPage2.Controls.Add(this.client_Box);
            this.tabPage2.Controls.Add(this.customer_Box);
            this.tabPage2.Controls.Add(this.job_Box);
            this.tabPage2.Controls.Add(this.label15);
            this.tabPage2.Controls.Add(this.label16);
            this.tabPage2.Controls.Add(this.mmHeight_Box);
            this.tabPage2.Controls.Add(this.label14);
            this.tabPage2.Controls.Add(this.depth_Box);
            this.tabPage2.Controls.Add(this.label13);
            this.tabPage2.Controls.Add(this.label12);
            this.tabPage2.Controls.Add(this.dia_Box);
            this.tabPage2.Controls.Add(this.label11);
            this.tabPage2.Controls.Add(this.lSPA_Box);
            this.tabPage2.Controls.Add(this.label10);
            this.tabPage2.Controls.Add(this.wSPA_Box);
            this.tabPage2.Controls.Add(this.label6);
            this.tabPage2.Controls.Add(this.bpLength_Box);
            this.tabPage2.Controls.Add(this.label5);
            this.tabPage2.Controls.Add(this.bpWidth_Box);
            this.tabPage2.Controls.Add(this.label4);
            this.tabPage2.Controls.Add(this.label3);
            this.tabPage2.Controls.Add(this.label2);
            this.tabPage2.Controls.Add(this.fanCount_Box);
            this.tabPage2.Controls.Add(this.label1);
            this.tabPage2.Controls.Add(this.midColumns_Box);
            this.tabPage2.Controls.Add(this.label9);
            this.tabPage2.Controls.Add(this.height_TextBox);
            this.tabPage2.Controls.Add(this.label8);
            this.tabPage2.Controls.Add(this.length_TextBox);
            this.tabPage2.Controls.Add(this.label7);
            this.tabPage2.Controls.Add(this.width_TextBox);
            this.tabPage2.Controls.Add(this.btn_Standard);
            this.tabPage2.Location = new System.Drawing.Point(4, 22);
            this.tabPage2.Name = "tabPage2";
            this.tabPage2.Size = new System.Drawing.Size(417, 522);
            this.tabPage2.TabIndex = 1;
            this.tabPage2.Text = "Structure";
            this.tabPage2.UseVisualStyleBackColor = true;
            // 
            // label53
            // 
            this.label53.AutoSize = true;
            this.label53.Location = new System.Drawing.Point(201, 189);
            this.label53.Name = "label53";
            this.label53.Size = new System.Drawing.Size(189, 13);
            this.label53.TabIndex = 124;
            this.label53.Text = "(bottom of MM to bottom of base plate)";
            // 
            // label52
            // 
            this.label52.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.label52.AutoSize = true;
            this.label52.Location = new System.Drawing.Point(123, 102);
            this.label52.Name = "label52";
            this.label52.Size = new System.Drawing.Size(203, 13);
            this.label52.TabIndex = 123;
            this.label52.Text = "(bottom of base plate to bottom of bundle)";
            // 
            // textBoxShipBeamHeight
            // 
            this.textBoxShipBeamHeight.Location = new System.Drawing.Point(195, 227);
            this.textBoxShipBeamHeight.Name = "textBoxShipBeamHeight";
            this.textBoxShipBeamHeight.Size = new System.Drawing.Size(100, 20);
            this.textBoxShipBeamHeight.TabIndex = 74;
            this.textBoxShipBeamHeight.TextChanged += new System.EventHandler(this.textBoxShipBeamHeight_TextChanged);
            // 
            // textBox_DriveWidth
            // 
            this.textBox_DriveWidth.Location = new System.Drawing.Point(228, 143);
            this.textBox_DriveWidth.Name = "textBox_DriveWidth";
            this.textBox_DriveWidth.Size = new System.Drawing.Size(100, 20);
            this.textBox_DriveWidth.TabIndex = 68;
            this.textBox_DriveWidth.TextChanged += new System.EventHandler(this.textBox_DriveWidth_TextChanged);
            // 
            // label48
            // 
            this.label48.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.label48.AutoSize = true;
            this.label48.Location = new System.Drawing.Point(334, 146);
            this.label48.Name = "label48";
            this.label48.Size = new System.Drawing.Size(35, 13);
            this.label48.TabIndex = 69;
            this.label48.Text = "Width";
            // 
            // label51
            // 
            this.label51.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.label51.AutoSize = true;
            this.label51.Location = new System.Drawing.Point(301, 230);
            this.label51.Name = "label51";
            this.label51.Size = new System.Drawing.Size(92, 13);
            this.label51.TabIndex = 73;
            this.label51.Text = "Ship Beam Height";
            // 
            // label49
            // 
            this.label49.AutoSize = true;
            this.label49.Location = new System.Drawing.Point(122, 357);
            this.label49.Name = "label49";
            this.label49.Size = new System.Drawing.Size(56, 13);
            this.label49.TabIndex = 71;
            this.label49.Text = "Thickness";
            // 
            // textBox_BasePlateTHK
            // 
            this.textBox_BasePlateTHK.Location = new System.Drawing.Point(16, 354);
            this.textBox_BasePlateTHK.Name = "textBox_BasePlateTHK";
            this.textBox_BasePlateTHK.Size = new System.Drawing.Size(100, 20);
            this.textBox_BasePlateTHK.TabIndex = 70;
            this.textBox_BasePlateTHK.TextChanged += new System.EventHandler(this.textBox_BasePlateTHK_TextChanged);
            // 
            // textBox_Bank
            // 
            this.textBox_Bank.Location = new System.Drawing.Point(369, 289);
            this.textBox_Bank.Name = "textBox_Bank";
            this.textBox_Bank.Size = new System.Drawing.Size(20, 20);
            this.textBox_Bank.TabIndex = 66;
            this.textBox_Bank.TextChanged += new System.EventHandler(this.textBox_Bank_TextChanged);
            // 
            // label47
            // 
            this.label47.AutoSize = true;
            this.label47.Location = new System.Drawing.Point(124, 441);
            this.label47.Name = "label47";
            this.label47.Size = new System.Drawing.Size(44, 13);
            this.label47.TabIndex = 65;
            this.label47.Text = "Material";
            // 
            // materialCombo
            // 
            this.materialCombo.FormattingEnabled = true;
            this.materialCombo.Items.AddRange(new object[] {
            "A36",
            "A572_50"});
            this.materialCombo.Location = new System.Drawing.Point(18, 438);
            this.materialCombo.Name = "materialCombo";
            this.materialCombo.Size = new System.Drawing.Size(100, 21);
            this.materialCombo.TabIndex = 63;
            this.materialCombo.SelectedIndexChanged += new System.EventHandler(this.materialCombo_SelectedIndexChanged);
            // 
            // label45
            // 
            this.label45.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.label45.AutoSize = true;
            this.label45.Location = new System.Drawing.Point(-43, 399);
            this.label45.Name = "label45";
            this.label45.Size = new System.Drawing.Size(44, 13);
            this.label45.TabIndex = 62;
            this.label45.Text = "Material";
            // 
            // button1_save
            // 
            this.button1_save.Location = new System.Drawing.Point(374, 3);
            this.button1_save.Name = "button1_save";
            this.button1_save.Size = new System.Drawing.Size(40, 24);
            this.button1_save.TabIndex = 61;
            this.button1_save.Text = "Save";
            this.button1_save.UseVisualStyleBackColor = true;
            this.button1_save.Click += new System.EventHandler(this.button1_save_Click);
            // 
            // beamSize_Box
            // 
            this.beamSize_Box.FormattingEnabled = true;
            this.beamSize_Box.Items.AddRange(new object[] {
            "W6x15",
            "W6x20",
            "W6x25",
            "W8x31"});
            this.beamSize_Box.Location = new System.Drawing.Point(18, 117);
            this.beamSize_Box.Name = "beamSize_Box";
            this.beamSize_Box.Size = new System.Drawing.Size(100, 21);
            this.beamSize_Box.TabIndex = 60;
            this.beamSize_Box.SelectedIndexChanged += new System.EventHandler(this.beamSize_Box_SelectedIndexChanged);
            // 
            // label19
            // 
            this.label19.AutoSize = true;
            this.label19.Location = new System.Drawing.Point(124, 121);
            this.label19.Name = "label19";
            this.label19.Size = new System.Drawing.Size(57, 13);
            this.label19.TabIndex = 59;
            this.label19.Text = "Beam Size";
            // 
            // label17
            // 
            this.label17.AutoSize = true;
            this.label17.Location = new System.Drawing.Point(126, 166);
            this.label17.Name = "label17";
            this.label17.Size = new System.Drawing.Size(58, 13);
            this.label17.TabIndex = 55;
            this.label17.Text = "Rotate 90°";
            // 
            // rotate_Box
            // 
            this.rotate_Box.AutoSize = true;
            this.rotate_Box.Location = new System.Drawing.Point(103, 165);
            this.rotate_Box.Name = "rotate_Box";
            this.rotate_Box.Size = new System.Drawing.Size(15, 14);
            this.rotate_Box.TabIndex = 54;
            this.rotate_Box.UseVisualStyleBackColor = true;
            this.rotate_Box.CheckedChanged += new System.EventHandler(this.rotate_Box_CheckedChanged);
            // 
            // initials_Box
            // 
            this.initials_Box.Location = new System.Drawing.Point(290, 448);
            this.initials_Box.Name = "initials_Box";
            this.initials_Box.Size = new System.Drawing.Size(100, 20);
            this.initials_Box.TabIndex = 53;
            this.initials_Box.TextChanged += new System.EventHandler(this.initials_Box_TextChanged);
            // 
            // itemNumber_Box
            // 
            this.itemNumber_Box.Location = new System.Drawing.Point(290, 419);
            this.itemNumber_Box.Name = "itemNumber_Box";
            this.itemNumber_Box.Size = new System.Drawing.Size(100, 20);
            this.itemNumber_Box.TabIndex = 52;
            this.itemNumber_Box.TextChanged += new System.EventHandler(this.itemNumber_Box_TextChanged);
            // 
            // purchaseOrder_Box
            // 
            this.purchaseOrder_Box.Location = new System.Drawing.Point(290, 393);
            this.purchaseOrder_Box.Name = "purchaseOrder_Box";
            this.purchaseOrder_Box.Size = new System.Drawing.Size(100, 20);
            this.purchaseOrder_Box.TabIndex = 51;
            this.purchaseOrder_Box.TextChanged += new System.EventHandler(this.purchaseOrder_Box_TextChanged);
            // 
            // location_Box
            // 
            this.location_Box.Location = new System.Drawing.Point(290, 367);
            this.location_Box.Name = "location_Box";
            this.location_Box.Size = new System.Drawing.Size(100, 20);
            this.location_Box.TabIndex = 50;
            this.location_Box.TextChanged += new System.EventHandler(this.location_Box_TextChanged);
            // 
            // client_Box
            // 
            this.client_Box.Location = new System.Drawing.Point(290, 341);
            this.client_Box.Name = "client_Box";
            this.client_Box.Size = new System.Drawing.Size(100, 20);
            this.client_Box.TabIndex = 49;
            this.client_Box.TextChanged += new System.EventHandler(this.client_Box_TextChanged);
            // 
            // customer_Box
            // 
            this.customer_Box.Location = new System.Drawing.Point(290, 315);
            this.customer_Box.Name = "customer_Box";
            this.customer_Box.Size = new System.Drawing.Size(100, 20);
            this.customer_Box.TabIndex = 48;
            this.customer_Box.TextChanged += new System.EventHandler(this.customer_Box_TextChanged);
            // 
            // job_Box
            // 
            this.job_Box.Location = new System.Drawing.Point(290, 289);
            this.job_Box.Name = "job_Box";
            this.job_Box.Size = new System.Drawing.Size(73, 20);
            this.job_Box.TabIndex = 47;
            this.job_Box.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            this.job_Box.TextChanged += new System.EventHandler(this.job_Box_TextChanged);
            // 
            // label15
            // 
            this.label15.AutoSize = true;
            this.label15.Location = new System.Drawing.Point(234, 127);
            this.label15.Name = "label15";
            this.label15.Size = new System.Drawing.Size(89, 13);
            this.label15.TabIndex = 46;
            this.label15.Text = "Machinery Mount";
            // 
            // label16
            // 
            this.label16.AutoSize = true;
            this.label16.Location = new System.Drawing.Point(333, 175);
            this.label16.Name = "label16";
            this.label16.Size = new System.Drawing.Size(56, 13);
            this.label16.TabIndex = 45;
            this.label16.Text = "Headroom";
            // 
            // mmHeight_Box
            // 
            this.mmHeight_Box.Location = new System.Drawing.Point(228, 169);
            this.mmHeight_Box.Name = "mmHeight_Box";
            this.mmHeight_Box.Size = new System.Drawing.Size(100, 20);
            this.mmHeight_Box.TabIndex = 44;
            this.mmHeight_Box.TextChanged += new System.EventHandler(this.mmHeight_Box_TextChanged);
            // 
            // label14
            // 
            this.label14.AutoSize = true;
            this.label14.Location = new System.Drawing.Point(334, 65);
            this.label14.Name = "label14";
            this.label14.Size = new System.Drawing.Size(36, 13);
            this.label14.TabIndex = 43;
            this.label14.Text = "Depth";
            // 
            // depth_Box
            // 
            this.depth_Box.Location = new System.Drawing.Point(228, 62);
            this.depth_Box.Name = "depth_Box";
            this.depth_Box.Size = new System.Drawing.Size(100, 20);
            this.depth_Box.TabIndex = 42;
            this.depth_Box.TextChanged += new System.EventHandler(this.depth_Box_TextChanged);
            // 
            // label13
            // 
            this.label13.AutoSize = true;
            this.label13.Location = new System.Drawing.Point(253, 18);
            this.label13.Name = "label13";
            this.label13.Size = new System.Drawing.Size(42, 13);
            this.label13.TabIndex = 41;
            this.label13.Text = "Plenum";
            // 
            // label12
            // 
            this.label12.AutoSize = true;
            this.label12.Location = new System.Drawing.Point(122, 331);
            this.label12.Name = "label12";
            this.label12.Size = new System.Drawing.Size(74, 13);
            this.label12.TabIndex = 40;
            this.label12.Text = "Hole Diameter";
            // 
            // dia_Box
            // 
            this.dia_Box.Location = new System.Drawing.Point(16, 328);
            this.dia_Box.Name = "dia_Box";
            this.dia_Box.Size = new System.Drawing.Size(100, 20);
            this.dia_Box.TabIndex = 39;
            this.dia_Box.TextChanged += new System.EventHandler(this.dia_Box_TextChanged);
            // 
            // label11
            // 
            this.label11.AutoSize = true;
            this.label11.Location = new System.Drawing.Point(122, 305);
            this.label11.Name = "label11";
            this.label11.Size = new System.Drawing.Size(153, 13);
            this.label11.TabIndex = 38;
            this.label11.Text = "Hole-to-Hole (along unit length)";
            // 
            // lSPA_Box
            // 
            this.lSPA_Box.Location = new System.Drawing.Point(16, 302);
            this.lSPA_Box.Name = "lSPA_Box";
            this.lSPA_Box.Size = new System.Drawing.Size(100, 20);
            this.lSPA_Box.TabIndex = 37;
            this.lSPA_Box.TextChanged += new System.EventHandler(this.lSPA_Box_TextChanged);
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(122, 279);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(149, 13);
            this.label10.TabIndex = 36;
            this.label10.Text = "Hole-to-Hole (along unit width)";
            // 
            // wSPA_Box
            // 
            this.wSPA_Box.Location = new System.Drawing.Point(16, 276);
            this.wSPA_Box.Name = "wSPA_Box";
            this.wSPA_Box.Size = new System.Drawing.Size(100, 20);
            this.wSPA_Box.TabIndex = 35;
            this.wSPA_Box.TextChanged += new System.EventHandler(this.wSPA_Box_TextChanged);
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(122, 253);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(40, 13);
            this.label6.TabIndex = 34;
            this.label6.Text = "Length";
            // 
            // bpLength_Box
            // 
            this.bpLength_Box.Location = new System.Drawing.Point(16, 250);
            this.bpLength_Box.Name = "bpLength_Box";
            this.bpLength_Box.Size = new System.Drawing.Size(100, 20);
            this.bpLength_Box.TabIndex = 33;
            this.bpLength_Box.TextChanged += new System.EventHandler(this.bpLength_Box_TextChanged);
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(122, 227);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(35, 13);
            this.label5.TabIndex = 32;
            this.label5.Text = "Width";
            // 
            // bpWidth_Box
            // 
            this.bpWidth_Box.Location = new System.Drawing.Point(16, 224);
            this.bpWidth_Box.Name = "bpWidth_Box";
            this.bpWidth_Box.Size = new System.Drawing.Size(100, 20);
            this.bpWidth_Box.TabIndex = 31;
            this.bpWidth_Box.TextChanged += new System.EventHandler(this.bpWidth_Box_TextChanged);
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(41, 208);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(58, 13);
            this.label4.TabIndex = 30;
            this.label4.Text = "Base Plate";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(43, 20);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(47, 13);
            this.label3.TabIndex = 29;
            this.label3.Text = "Columns";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(334, 39);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(56, 13);
            this.label2.TabIndex = 28;
            this.label2.Text = "Fan Count";
            // 
            // fanCount_Box
            // 
            this.fanCount_Box.Location = new System.Drawing.Point(228, 36);
            this.fanCount_Box.Name = "fanCount_Box";
            this.fanCount_Box.Size = new System.Drawing.Size(100, 20);
            this.fanCount_Box.TabIndex = 27;
            this.fanCount_Box.TextChanged += new System.EventHandler(this.fanCount_Box_TextChanged);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(126, 145);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(67, 13);
            this.label1.TabIndex = 26;
            this.label1.Text = "Mid Columns";
            // 
            // midColumns_Box
            // 
            this.midColumns_Box.AutoSize = true;
            this.midColumns_Box.Location = new System.Drawing.Point(103, 144);
            this.midColumns_Box.Name = "midColumns_Box";
            this.midColumns_Box.Size = new System.Drawing.Size(15, 14);
            this.midColumns_Box.TabIndex = 25;
            this.midColumns_Box.UseVisualStyleBackColor = true;
            this.midColumns_Box.CheckedChanged += new System.EventHandler(this.midColumn_Box_CheckedChanged);
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(123, 91);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(103, 13);
            this.label9.TabIndex = 24;
            this.label9.Text = "Total Column Height";
            // 
            // height_TextBox
            // 
            this.height_TextBox.Location = new System.Drawing.Point(18, 92);
            this.height_TextBox.Name = "height_TextBox";
            this.height_TextBox.Size = new System.Drawing.Size(100, 20);
            this.height_TextBox.TabIndex = 23;
            this.height_TextBox.TextChanged += new System.EventHandler(this.height_TextBox_TextChanged);
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(124, 69);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(76, 13);
            this.label8.TabIndex = 22;
            this.label8.Text = "Length (cL-cL)";
            // 
            // length_TextBox
            // 
            this.length_TextBox.Location = new System.Drawing.Point(18, 66);
            this.length_TextBox.Name = "length_TextBox";
            this.length_TextBox.Size = new System.Drawing.Size(100, 20);
            this.length_TextBox.TabIndex = 21;
            this.length_TextBox.TextChanged += new System.EventHandler(this.length_TextBox_TextChanged);
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(124, 43);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(71, 13);
            this.label7.TabIndex = 20;
            this.label7.Text = "Width (cL-cL)";
            // 
            // width_TextBox
            // 
            this.width_TextBox.Location = new System.Drawing.Point(18, 40);
            this.width_TextBox.Name = "width_TextBox";
            this.width_TextBox.Size = new System.Drawing.Size(100, 20);
            this.width_TextBox.TabIndex = 19;
            this.width_TextBox.TextChanged += new System.EventHandler(this.width_TextBox_TextChanged);
            // 
            // btn_Standard
            // 
            this.btn_Standard.Location = new System.Drawing.Point(164, 484);
            this.btn_Standard.Name = "btn_Standard";
            this.btn_Standard.Size = new System.Drawing.Size(85, 24);
            this.btn_Standard.TabIndex = 18;
            this.btn_Standard.Text = "Structure";
            this.btn_Standard.UseVisualStyleBackColor = true;
            this.btn_Standard.Click += new System.EventHandler(this.btn_Standard_Click);
            // 
            // tabPage3
            // 
            this.tabPage3.Controls.Add(this.label50);
            this.tabPage3.Controls.Add(this.textBox_ClipHeight);
            this.tabPage3.Controls.Add(this.button1);
            this.tabPage3.Controls.Add(this.label44);
            this.tabPage3.Controls.Add(this.label42);
            this.tabPage3.Controls.Add(this.flangeGageWT_Box);
            this.tabPage3.Controls.Add(this.label43);
            this.tabPage3.Controls.Add(this.k1WT_Box);
            this.tabPage3.Controls.Add(this.label37);
            this.tabPage3.Controls.Add(this.kWT_Box);
            this.tabPage3.Controls.Add(this.label38);
            this.tabPage3.Controls.Add(this.flangeTHKWT_Box);
            this.tabPage3.Controls.Add(this.label39);
            this.tabPage3.Controls.Add(this.flangeWidthWT_Box);
            this.tabPage3.Controls.Add(this.label40);
            this.tabPage3.Controls.Add(this.stemTHKWT_Box);
            this.tabPage3.Controls.Add(this.label41);
            this.tabPage3.Controls.Add(this.depthWT_Box);
            this.tabPage3.Controls.Add(this.label36);
            this.tabPage3.Controls.Add(this.label35);
            this.tabPage3.Controls.Add(this.kL_Box);
            this.tabPage3.Controls.Add(this.label34);
            this.tabPage3.Controls.Add(this.thkL_Box);
            this.tabPage3.Controls.Add(this.label33);
            this.tabPage3.Controls.Add(this.gage_Box);
            this.tabPage3.Controls.Add(this.label24);
            this.tabPage3.Controls.Add(this.leg2_Box);
            this.tabPage3.Controls.Add(this.label23);
            this.tabPage3.Controls.Add(this.leg1_Box);
            this.tabPage3.Controls.Add(this.label25);
            this.tabPage3.Controls.Add(this.clipTHK_Box);
            this.tabPage3.Controls.Add(this.label22);
            this.tabPage3.Controls.Add(this.braceAngle_Box);
            this.tabPage3.Controls.Add(this.label21);
            this.tabPage3.Controls.Add(this.braceHoleDiameter_Box);
            this.tabPage3.Controls.Add(this.label20);
            this.tabPage3.Controls.Add(this.label18);
            this.tabPage3.Controls.Add(this.braceType_Box);
            this.tabPage3.Location = new System.Drawing.Point(4, 22);
            this.tabPage3.Name = "tabPage3";
            this.tabPage3.Size = new System.Drawing.Size(417, 522);
            this.tabPage3.TabIndex = 3;
            this.tabPage3.Text = "Braces";
            this.tabPage3.UseVisualStyleBackColor = true;
            // 
            // label50
            // 
            this.label50.AutoSize = true;
            this.label50.Location = new System.Drawing.Point(240, 92);
            this.label50.Name = "label50";
            this.label50.Size = new System.Drawing.Size(131, 13);
            this.label50.TabIndex = 109;
            this.label50.Text = "Clip Height (L and LL only)";
            this.label50.Click += new System.EventHandler(this.label50_Click);
            // 
            // textBox_ClipHeight
            // 
            this.textBox_ClipHeight.Location = new System.Drawing.Point(134, 89);
            this.textBox_ClipHeight.Name = "textBox_ClipHeight";
            this.textBox_ClipHeight.Size = new System.Drawing.Size(100, 20);
            this.textBox_ClipHeight.TabIndex = 108;
            this.textBox_ClipHeight.TextChanged += new System.EventHandler(this.textBox_ClipHeight_TextChanged);
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(149, 485);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(85, 24);
            this.button1.TabIndex = 107;
            this.button1.Text = "Structure";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Click += new System.EventHandler(this.button1_Click);
            // 
            // label44
            // 
            this.label44.AutoSize = true;
            this.label44.Location = new System.Drawing.Point(254, 197);
            this.label44.Name = "label44";
            this.label44.Size = new System.Drawing.Size(25, 13);
            this.label44.TabIndex = 106;
            this.label44.Text = "WT";
            // 
            // label42
            // 
            this.label42.AutoSize = true;
            this.label42.Location = new System.Drawing.Point(326, 379);
            this.label42.Name = "label42";
            this.label42.Size = new System.Drawing.Size(68, 13);
            this.label42.TabIndex = 105;
            this.label42.Text = "Flange Gage";
            // 
            // flangeGageWT_Box
            // 
            this.flangeGageWT_Box.Location = new System.Drawing.Point(220, 376);
            this.flangeGageWT_Box.Name = "flangeGageWT_Box";
            this.flangeGageWT_Box.Size = new System.Drawing.Size(100, 20);
            this.flangeGageWT_Box.TabIndex = 104;
            this.flangeGageWT_Box.TextChanged += new System.EventHandler(this.flangeGageWT_Box_TextChanged);
            // 
            // label43
            // 
            this.label43.AutoSize = true;
            this.label43.Location = new System.Drawing.Point(326, 353);
            this.label43.Name = "label43";
            this.label43.Size = new System.Drawing.Size(20, 13);
            this.label43.TabIndex = 103;
            this.label43.Text = "K1";
            // 
            // k1WT_Box
            // 
            this.k1WT_Box.Location = new System.Drawing.Point(220, 350);
            this.k1WT_Box.Name = "k1WT_Box";
            this.k1WT_Box.Size = new System.Drawing.Size(100, 20);
            this.k1WT_Box.TabIndex = 102;
            this.k1WT_Box.TextChanged += new System.EventHandler(this.k1WT_Box_TextChanged);
            // 
            // label37
            // 
            this.label37.AutoSize = true;
            this.label37.Location = new System.Drawing.Point(326, 327);
            this.label37.Name = "label37";
            this.label37.Size = new System.Drawing.Size(14, 13);
            this.label37.TabIndex = 101;
            this.label37.Text = "K";
            // 
            // kWT_Box
            // 
            this.kWT_Box.Location = new System.Drawing.Point(220, 324);
            this.kWT_Box.Name = "kWT_Box";
            this.kWT_Box.Size = new System.Drawing.Size(100, 20);
            this.kWT_Box.TabIndex = 100;
            this.kWT_Box.TextChanged += new System.EventHandler(this.kWT_Box_TextChanged);
            // 
            // label38
            // 
            this.label38.AutoSize = true;
            this.label38.Location = new System.Drawing.Point(326, 301);
            this.label38.Name = "label38";
            this.label38.Size = new System.Drawing.Size(64, 13);
            this.label38.TabIndex = 99;
            this.label38.Text = "Flange THK";
            // 
            // flangeTHKWT_Box
            // 
            this.flangeTHKWT_Box.Location = new System.Drawing.Point(220, 298);
            this.flangeTHKWT_Box.Name = "flangeTHKWT_Box";
            this.flangeTHKWT_Box.Size = new System.Drawing.Size(100, 20);
            this.flangeTHKWT_Box.TabIndex = 98;
            this.flangeTHKWT_Box.TextChanged += new System.EventHandler(this.flangeTHKWT_Box_TextChanged);
            // 
            // label39
            // 
            this.label39.AutoSize = true;
            this.label39.Location = new System.Drawing.Point(326, 275);
            this.label39.Name = "label39";
            this.label39.Size = new System.Drawing.Size(70, 13);
            this.label39.TabIndex = 97;
            this.label39.Text = "Flange Width";
            // 
            // flangeWidthWT_Box
            // 
            this.flangeWidthWT_Box.Location = new System.Drawing.Point(220, 272);
            this.flangeWidthWT_Box.Name = "flangeWidthWT_Box";
            this.flangeWidthWT_Box.Size = new System.Drawing.Size(100, 20);
            this.flangeWidthWT_Box.TabIndex = 96;
            this.flangeWidthWT_Box.TextChanged += new System.EventHandler(this.flangeWidthWT_Box_TextChanged);
            // 
            // label40
            // 
            this.label40.AutoSize = true;
            this.label40.Location = new System.Drawing.Point(326, 249);
            this.label40.Name = "label40";
            this.label40.Size = new System.Drawing.Size(53, 13);
            this.label40.TabIndex = 95;
            this.label40.Text = "StemTHK";
            // 
            // stemTHKWT_Box
            // 
            this.stemTHKWT_Box.Location = new System.Drawing.Point(220, 246);
            this.stemTHKWT_Box.Name = "stemTHKWT_Box";
            this.stemTHKWT_Box.Size = new System.Drawing.Size(100, 20);
            this.stemTHKWT_Box.TabIndex = 94;
            this.stemTHKWT_Box.TextChanged += new System.EventHandler(this.stemTHKWT_Box_TextChanged);
            // 
            // label41
            // 
            this.label41.AutoSize = true;
            this.label41.Location = new System.Drawing.Point(326, 223);
            this.label41.Name = "label41";
            this.label41.Size = new System.Drawing.Size(36, 13);
            this.label41.TabIndex = 93;
            this.label41.Text = "Depth";
            // 
            // depthWT_Box
            // 
            this.depthWT_Box.Location = new System.Drawing.Point(220, 220);
            this.depthWT_Box.Name = "depthWT_Box";
            this.depthWT_Box.Size = new System.Drawing.Size(100, 20);
            this.depthWT_Box.TabIndex = 92;
            this.depthWT_Box.TextChanged += new System.EventHandler(this.depthWT_Box_TextChanged);
            // 
            // label36
            // 
            this.label36.AutoSize = true;
            this.label36.Location = new System.Drawing.Point(64, 197);
            this.label36.Name = "label36";
            this.label36.Size = new System.Drawing.Size(13, 13);
            this.label36.TabIndex = 91;
            this.label36.Text = "L";
            // 
            // label35
            // 
            this.label35.AutoSize = true;
            this.label35.Location = new System.Drawing.Point(131, 327);
            this.label35.Name = "label35";
            this.label35.Size = new System.Drawing.Size(14, 13);
            this.label35.TabIndex = 90;
            this.label35.Text = "K";
            // 
            // kL_Box
            // 
            this.kL_Box.Location = new System.Drawing.Point(25, 324);
            this.kL_Box.Name = "kL_Box";
            this.kL_Box.Size = new System.Drawing.Size(100, 20);
            this.kL_Box.TabIndex = 89;
            this.kL_Box.TextChanged += new System.EventHandler(this.kL_Box_TextChanged);
            // 
            // label34
            // 
            this.label34.AutoSize = true;
            this.label34.Location = new System.Drawing.Point(131, 301);
            this.label34.Name = "label34";
            this.label34.Size = new System.Drawing.Size(29, 13);
            this.label34.TabIndex = 88;
            this.label34.Text = "THK";
            // 
            // thkL_Box
            // 
            this.thkL_Box.Location = new System.Drawing.Point(25, 298);
            this.thkL_Box.Name = "thkL_Box";
            this.thkL_Box.Size = new System.Drawing.Size(100, 20);
            this.thkL_Box.TabIndex = 87;
            this.thkL_Box.TextChanged += new System.EventHandler(this.thk_Box_TextChanged);
            // 
            // label33
            // 
            this.label33.AutoSize = true;
            this.label33.Location = new System.Drawing.Point(131, 275);
            this.label33.Name = "label33";
            this.label33.Size = new System.Drawing.Size(33, 13);
            this.label33.TabIndex = 86;
            this.label33.Text = "Gage";
            // 
            // gage_Box
            // 
            this.gage_Box.Location = new System.Drawing.Point(25, 272);
            this.gage_Box.Name = "gage_Box";
            this.gage_Box.Size = new System.Drawing.Size(100, 20);
            this.gage_Box.TabIndex = 85;
            this.gage_Box.TextChanged += new System.EventHandler(this.gage_Box_TextChanged);
            // 
            // label24
            // 
            this.label24.AutoSize = true;
            this.label24.Location = new System.Drawing.Point(131, 249);
            this.label24.Name = "label24";
            this.label24.Size = new System.Drawing.Size(31, 13);
            this.label24.TabIndex = 84;
            this.label24.Text = "Leg2";
            // 
            // leg2_Box
            // 
            this.leg2_Box.Location = new System.Drawing.Point(25, 246);
            this.leg2_Box.Name = "leg2_Box";
            this.leg2_Box.Size = new System.Drawing.Size(100, 20);
            this.leg2_Box.TabIndex = 83;
            this.leg2_Box.TextChanged += new System.EventHandler(this.leg2_Box_TextChanged);
            // 
            // label23
            // 
            this.label23.AutoSize = true;
            this.label23.Location = new System.Drawing.Point(131, 223);
            this.label23.Name = "label23";
            this.label23.Size = new System.Drawing.Size(31, 13);
            this.label23.TabIndex = 82;
            this.label23.Text = "Leg1";
            // 
            // leg1_Box
            // 
            this.leg1_Box.Location = new System.Drawing.Point(25, 220);
            this.leg1_Box.Name = "leg1_Box";
            this.leg1_Box.Size = new System.Drawing.Size(100, 20);
            this.leg1_Box.TabIndex = 81;
            this.leg1_Box.TextChanged += new System.EventHandler(this.leg1_Box_TextChanged);
            // 
            // label25
            // 
            this.label25.AutoSize = true;
            this.label25.Location = new System.Drawing.Point(240, 66);
            this.label25.Name = "label25";
            this.label25.Size = new System.Drawing.Size(76, 13);
            this.label25.TabIndex = 80;
            this.label25.Text = "Clip Thickness";
            // 
            // clipTHK_Box
            // 
            this.clipTHK_Box.Location = new System.Drawing.Point(134, 63);
            this.clipTHK_Box.Name = "clipTHK_Box";
            this.clipTHK_Box.Size = new System.Drawing.Size(100, 20);
            this.clipTHK_Box.TabIndex = 79;
            this.clipTHK_Box.TextChanged += new System.EventHandler(this.clipTHK_Box_TextChanged);
            // 
            // label22
            // 
            this.label22.AutoSize = true;
            this.label22.Location = new System.Drawing.Point(240, 144);
            this.label22.Name = "label22";
            this.label22.Size = new System.Drawing.Size(34, 13);
            this.label22.TabIndex = 78;
            this.label22.Text = "Angle";
            // 
            // braceAngle_Box
            // 
            this.braceAngle_Box.Location = new System.Drawing.Point(134, 141);
            this.braceAngle_Box.Name = "braceAngle_Box";
            this.braceAngle_Box.Size = new System.Drawing.Size(100, 20);
            this.braceAngle_Box.TabIndex = 77;
            this.braceAngle_Box.TextChanged += new System.EventHandler(this.braceAngle_Box_TextChanged_1);
            // 
            // label21
            // 
            this.label21.AutoSize = true;
            this.label21.Location = new System.Drawing.Point(240, 118);
            this.label21.Name = "label21";
            this.label21.Size = new System.Drawing.Size(74, 13);
            this.label21.TabIndex = 76;
            this.label21.Text = "Hole Diameter";
            // 
            // braceHoleDiameter_Box
            // 
            this.braceHoleDiameter_Box.Location = new System.Drawing.Point(134, 115);
            this.braceHoleDiameter_Box.Name = "braceHoleDiameter_Box";
            this.braceHoleDiameter_Box.Size = new System.Drawing.Size(100, 20);
            this.braceHoleDiameter_Box.TabIndex = 75;
            this.braceHoleDiameter_Box.TextChanged += new System.EventHandler(this.braceHoleDiameter_Box_TextChanged_1);
            // 
            // label20
            // 
            this.label20.AutoSize = true;
            this.label20.Location = new System.Drawing.Point(240, 40);
            this.label20.Name = "label20";
            this.label20.Size = new System.Drawing.Size(31, 13);
            this.label20.TabIndex = 74;
            this.label20.Text = "Type";
            // 
            // label18
            // 
            this.label18.AutoSize = true;
            this.label18.Location = new System.Drawing.Point(160, 18);
            this.label18.Name = "label18";
            this.label18.Size = new System.Drawing.Size(40, 13);
            this.label18.TabIndex = 73;
            this.label18.Text = "Braces";
            // 
            // braceType_Box
            // 
            this.braceType_Box.FormattingEnabled = true;
            this.braceType_Box.Items.AddRange(new object[] {
            "L",
            "LL",
            "T",
            "X",
            "TX"});
            this.braceType_Box.Location = new System.Drawing.Point(134, 36);
            this.braceType_Box.Name = "braceType_Box";
            this.braceType_Box.Size = new System.Drawing.Size(100, 21);
            this.braceType_Box.TabIndex = 72;
            this.braceType_Box.SelectedIndexChanged += new System.EventHandler(this.braceType_Box_SelectedIndexChanged_1);
            // 
            // tabPage1
            // 
            this.tabPage1.Controls.Add(this.delete_Toggle);
            this.tabPage1.Controls.Add(this.save_Toggle);
            this.tabPage1.Controls.Add(this.createDrawing_Toggle);
            this.tabPage1.Controls.Add(this.label26);
            this.tabPage1.Controls.Add(this.textBox_K1);
            this.tabPage1.Controls.Add(this.textBox_K);
            this.tabPage1.Controls.Add(this.textBox_FlangeTHK);
            this.tabPage1.Controls.Add(this.textBox_FlangeWidth);
            this.tabPage1.Controls.Add(this.textBox_WebTHK);
            this.tabPage1.Controls.Add(this.textBox_Depth);
            this.tabPage1.Controls.Add(this.label27);
            this.tabPage1.Controls.Add(this.label28);
            this.tabPage1.Controls.Add(this.label29);
            this.tabPage1.Controls.Add(this.label30);
            this.tabPage1.Controls.Add(this.label31);
            this.tabPage1.Controls.Add(this.label32);
            this.tabPage1.Location = new System.Drawing.Point(4, 22);
            this.tabPage1.Name = "tabPage1";
            this.tabPage1.Size = new System.Drawing.Size(417, 522);
            this.tabPage1.TabIndex = 2;
            this.tabPage1.Text = "Advanced";
            this.tabPage1.UseVisualStyleBackColor = true;
            // 
            // delete_Toggle
            // 
            this.delete_Toggle.AutoSize = true;
            this.delete_Toggle.Location = new System.Drawing.Point(137, 322);
            this.delete_Toggle.Name = "delete_Toggle";
            this.delete_Toggle.Size = new System.Drawing.Size(128, 17);
            this.delete_Toggle.TabIndex = 35;
            this.delete_Toggle.Text = "Delete Uneeded Files";
            this.delete_Toggle.UseVisualStyleBackColor = true;
            this.delete_Toggle.CheckedChanged += new System.EventHandler(this.checkBox4_delete_CheckedChanged);
            // 
            // save_Toggle
            // 
            this.save_Toggle.AutoSize = true;
            this.save_Toggle.Location = new System.Drawing.Point(137, 299);
            this.save_Toggle.Name = "save_Toggle";
            this.save_Toggle.Size = new System.Drawing.Size(76, 17);
            this.save_Toggle.TabIndex = 34;
            this.save_Toggle.Text = "Auto Save";
            this.save_Toggle.UseVisualStyleBackColor = true;
            this.save_Toggle.CheckedChanged += new System.EventHandler(this.checkBox3_save_CheckedChanged);
            // 
            // createDrawing_Toggle
            // 
            this.createDrawing_Toggle.AutoSize = true;
            this.createDrawing_Toggle.Location = new System.Drawing.Point(137, 276);
            this.createDrawing_Toggle.Name = "createDrawing_Toggle";
            this.createDrawing_Toggle.Size = new System.Drawing.Size(118, 17);
            this.createDrawing_Toggle.TabIndex = 33;
            this.createDrawing_Toggle.Text = "Create Drawing File";
            this.createDrawing_Toggle.UseVisualStyleBackColor = true;
            this.createDrawing_Toggle.CheckedChanged += new System.EventHandler(this.checkBox2_dwg_CheckedChanged);
            // 
            // label26
            // 
            this.label26.AutoSize = true;
            this.label26.Location = new System.Drawing.Point(162, 28);
            this.label26.Name = "label26";
            this.label26.Size = new System.Drawing.Size(113, 13);
            this.label26.TabIndex = 25;
            this.label26.Text = "Column Size Overrides";
            // 
            // textBox_K1
            // 
            this.textBox_K1.Location = new System.Drawing.Point(169, 182);
            this.textBox_K1.Name = "textBox_K1";
            this.textBox_K1.Size = new System.Drawing.Size(100, 20);
            this.textBox_K1.TabIndex = 24;
            this.textBox_K1.TextChanged += new System.EventHandler(this.textBox_K1_TextChanged);
            // 
            // textBox_K
            // 
            this.textBox_K.Location = new System.Drawing.Point(169, 156);
            this.textBox_K.Name = "textBox_K";
            this.textBox_K.Size = new System.Drawing.Size(100, 20);
            this.textBox_K.TabIndex = 22;
            this.textBox_K.TextChanged += new System.EventHandler(this.textBox_K_TextChanged);
            // 
            // textBox_FlangeTHK
            // 
            this.textBox_FlangeTHK.Location = new System.Drawing.Point(169, 130);
            this.textBox_FlangeTHK.Name = "textBox_FlangeTHK";
            this.textBox_FlangeTHK.Size = new System.Drawing.Size(100, 20);
            this.textBox_FlangeTHK.TabIndex = 20;
            this.textBox_FlangeTHK.TextChanged += new System.EventHandler(this.textBox_FlangeTHK_TextChanged);
            // 
            // textBox_FlangeWidth
            // 
            this.textBox_FlangeWidth.Location = new System.Drawing.Point(169, 104);
            this.textBox_FlangeWidth.Name = "textBox_FlangeWidth";
            this.textBox_FlangeWidth.Size = new System.Drawing.Size(100, 20);
            this.textBox_FlangeWidth.TabIndex = 18;
            this.textBox_FlangeWidth.TextChanged += new System.EventHandler(this.textBox_FlangeWidth_TextChanged);
            // 
            // textBox_WebTHK
            // 
            this.textBox_WebTHK.Location = new System.Drawing.Point(169, 78);
            this.textBox_WebTHK.Name = "textBox_WebTHK";
            this.textBox_WebTHK.Size = new System.Drawing.Size(100, 20);
            this.textBox_WebTHK.TabIndex = 16;
            this.textBox_WebTHK.TextChanged += new System.EventHandler(this.textBox_WebTHK_TextChanged);
            // 
            // textBox_Depth
            // 
            this.textBox_Depth.Location = new System.Drawing.Point(169, 52);
            this.textBox_Depth.Name = "textBox_Depth";
            this.textBox_Depth.Size = new System.Drawing.Size(100, 20);
            this.textBox_Depth.TabIndex = 14;
            this.textBox_Depth.TextChanged += new System.EventHandler(this.textBox_Depth_TextChanged);
            // 
            // label27
            // 
            this.label27.AutoSize = true;
            this.label27.Location = new System.Drawing.Point(135, 185);
            this.label27.Name = "label27";
            this.label27.Size = new System.Drawing.Size(20, 13);
            this.label27.TabIndex = 23;
            this.label27.Text = "K1";
            // 
            // label28
            // 
            this.label28.AutoSize = true;
            this.label28.Location = new System.Drawing.Point(141, 159);
            this.label28.Name = "label28";
            this.label28.Size = new System.Drawing.Size(14, 13);
            this.label28.TabIndex = 21;
            this.label28.Text = "K";
            // 
            // label29
            // 
            this.label29.AutoSize = true;
            this.label29.Location = new System.Drawing.Point(94, 133);
            this.label29.Name = "label29";
            this.label29.Size = new System.Drawing.Size(61, 13);
            this.label29.TabIndex = 19;
            this.label29.Text = "FlangeTHK";
            // 
            // label30
            // 
            this.label30.AutoSize = true;
            this.label30.Location = new System.Drawing.Point(85, 107);
            this.label30.Name = "label30";
            this.label30.Size = new System.Drawing.Size(70, 13);
            this.label30.TabIndex = 17;
            this.label30.Text = "Flange Width";
            // 
            // label31
            // 
            this.label31.AutoSize = true;
            this.label31.Location = new System.Drawing.Point(103, 81);
            this.label31.Name = "label31";
            this.label31.Size = new System.Drawing.Size(52, 13);
            this.label31.TabIndex = 15;
            this.label31.Text = "WebTHK";
            // 
            // label32
            // 
            this.label32.AutoSize = true;
            this.label32.Location = new System.Drawing.Point(119, 55);
            this.label32.Name = "label32";
            this.label32.Size = new System.Drawing.Size(36, 13);
            this.label32.TabIndex = 13;
            this.label32.Text = "Depth";
            // 
            // StructureUI
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(452, 572);
            this.Controls.Add(this.txt_HandrailBank);
            this.Name = "StructureUI";
            this.Text = "StructureUI";
            this.Load += new System.EventHandler(this.StructureUI_Load);
            this.txt_HandrailBank.ResumeLayout(false);
            this.tabPage2.ResumeLayout(false);
            this.tabPage2.PerformLayout();
            this.tabPage3.ResumeLayout(false);
            this.tabPage3.PerformLayout();
            this.tabPage1.ResumeLayout(false);
            this.tabPage1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion
        private System.Windows.Forms.TabControl txt_HandrailBank;
        private System.Windows.Forms.TabPage tabPage2;
        private System.Windows.Forms.Button btn_Standard;
        private System.Windows.Forms.TextBox width_TextBox;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.TextBox height_TextBox;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.TextBox length_TextBox;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox fanCount_Box;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.CheckBox midColumns_Box;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label label12;
        private System.Windows.Forms.TextBox dia_Box;
        private System.Windows.Forms.Label label11;
        private System.Windows.Forms.TextBox lSPA_Box;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.TextBox wSPA_Box;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.TextBox bpLength_Box;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.TextBox bpWidth_Box;
        private System.Windows.Forms.Label label13;
        private System.Windows.Forms.Label label15;
        private System.Windows.Forms.Label label16;
        private System.Windows.Forms.TextBox mmHeight_Box;
        private System.Windows.Forms.Label label14;
        private System.Windows.Forms.TextBox depth_Box;
        private System.Windows.Forms.TextBox initials_Box;
        private System.Windows.Forms.TextBox itemNumber_Box;
        private System.Windows.Forms.TextBox purchaseOrder_Box;
        private System.Windows.Forms.TextBox location_Box;
        private System.Windows.Forms.TextBox client_Box;
        private System.Windows.Forms.TextBox customer_Box;
        private System.Windows.Forms.TextBox job_Box;
        private System.Windows.Forms.Label label17;
        private System.Windows.Forms.CheckBox rotate_Box;
        private System.Windows.Forms.ComboBox beamSize_Box;
        private System.Windows.Forms.Label label19;
        private System.Windows.Forms.TabPage tabPage1;
        private System.Windows.Forms.Label label26;
        private System.Windows.Forms.TextBox textBox_K1;
        private System.Windows.Forms.TextBox textBox_K;
        private System.Windows.Forms.TextBox textBox_FlangeTHK;
        private System.Windows.Forms.TextBox textBox_FlangeWidth;
        private System.Windows.Forms.TextBox textBox_WebTHK;
        private System.Windows.Forms.TextBox textBox_Depth;
        private System.Windows.Forms.Label label27;
        private System.Windows.Forms.Label label28;
        private System.Windows.Forms.Label label29;
        private System.Windows.Forms.Label label30;
        private System.Windows.Forms.Label label31;
        private System.Windows.Forms.Label label32;
        private System.Windows.Forms.TabPage tabPage3;
        private System.Windows.Forms.Label label25;
        private System.Windows.Forms.TextBox clipTHK_Box;
        private System.Windows.Forms.Label label22;
        private System.Windows.Forms.TextBox braceAngle_Box;
        private System.Windows.Forms.Label label21;
        private System.Windows.Forms.TextBox braceHoleDiameter_Box;
        private System.Windows.Forms.Label label20;
        private System.Windows.Forms.Label label18;
        private System.Windows.Forms.ComboBox braceType_Box;
        private System.Windows.Forms.Label label36;
        private System.Windows.Forms.Label label35;
        private System.Windows.Forms.TextBox kL_Box;
        private System.Windows.Forms.Label label34;
        private System.Windows.Forms.TextBox thkL_Box;
        private System.Windows.Forms.Label label33;
        private System.Windows.Forms.TextBox gage_Box;
        private System.Windows.Forms.Label label24;
        private System.Windows.Forms.TextBox leg2_Box;
        private System.Windows.Forms.Label label23;
        private System.Windows.Forms.TextBox leg1_Box;
        private System.Windows.Forms.Label label44;
        private System.Windows.Forms.Label label42;
        private System.Windows.Forms.TextBox flangeGageWT_Box;
        private System.Windows.Forms.Label label43;
        private System.Windows.Forms.TextBox k1WT_Box;
        private System.Windows.Forms.Label label37;
        private System.Windows.Forms.TextBox kWT_Box;
        private System.Windows.Forms.Label label38;
        private System.Windows.Forms.TextBox flangeTHKWT_Box;
        private System.Windows.Forms.Label label39;
        private System.Windows.Forms.TextBox flangeWidthWT_Box;
        private System.Windows.Forms.Label label40;
        private System.Windows.Forms.TextBox stemTHKWT_Box;
        private System.Windows.Forms.Label label41;
        private System.Windows.Forms.TextBox depthWT_Box;
        private System.Windows.Forms.Button button1;
        private System.Windows.Forms.Button button1_save;
        private System.Windows.Forms.CheckBox delete_Toggle;
        private System.Windows.Forms.CheckBox save_Toggle;
        private System.Windows.Forms.CheckBox createDrawing_Toggle;
        private System.Windows.Forms.Label label47;
        private System.Windows.Forms.ComboBox materialCombo;
        private System.Windows.Forms.Label label45;
        private System.Windows.Forms.TextBox textBox_Bank;
        private System.Windows.Forms.TextBox textBox_DriveWidth;
        private System.Windows.Forms.Label label48;
        private System.Windows.Forms.Label label49;
        private System.Windows.Forms.TextBox textBox_BasePlateTHK;
        private System.Windows.Forms.Label label50;
        private System.Windows.Forms.TextBox textBox_ClipHeight;
        private System.Windows.Forms.Label label51;
        private System.Windows.Forms.TextBox textBoxShipBeamHeight;
        private System.Windows.Forms.Label label52;
        private System.Windows.Forms.Label label53;
    }
}