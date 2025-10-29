using FileTools.Base;
using System;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;
using static FileTools.CommonData.CommonData;
using static Excel.Prego;
using HDR.Box;
using HDR.Box.Derived;
using HDR.Connections;
using static FileTools.Properties.Settings;
using HDR.Connections.Derived;
using static HDR.Drawings.TitleBlock;

namespace HDR
{
    public class HeaderBase : MainAssembly
    {
        // Static properties
        public static IHeaderExtensions Header { get; set; }
        static public double ModelLengthReduction => Default.Stainless ? 4 : 6;
        static public IHeaderExtensions LowestLeftHeader
        {
            get
            {
                IHeaderExtensions lowestHeader;
                if (Header65.IsRequired)
                    lowestHeader = Header65;
                else if (Header63.IsRequired)
                    lowestHeader = Header63;
                else lowestHeader = Header61;
                return lowestHeader;
            }
        }
        static public IHeaderExtensions LowestRightHeader
        {
            get
            {
                IHeaderExtensions lowestHeader;
                if (Header66.IsRequired)
                    lowestHeader = Header66;
                else if (Header64.IsRequired)
                    lowestHeader = Header64;
                else lowestHeader = Header62;
                return lowestHeader;
            }
        }


        // Constructor
        public HeaderBase(int assemblyNumber, string assemblyDescription) : base(assemblyNumber, assemblyDescription
            //, typeof(EndPlate2)
            )
        {
            Flange.ClearPositionData();
            Extension.ClearPositionData();
            Partition.ClearPositionData();
            Stiffener.ClearPositionData();
        }


        // Debugging
        [STAThread]
        static void Main()
        {
            try
            {
                Application.EnableVisualStyles();
                Application.SetCompatibleTextRenderingDefault(false);
                
                // Launch the new modern tabbed interface
                Application.Run(new ModernHeaderForm());
                
                // To use the classic styled version, uncomment below:
                // var headerUI = new HeaderUI();
                // ApplyModernStyling(headerUI);
                // Application.Run(headerUI);
            }
            catch (Exception ex)
            {
                MessageBox.Show($"STARTUP ERROR:\n\n{ex.Message}\n\nStack Trace:\n{ex.StackTrace}", 
                    "CHART Header Tool - Error", 
                    MessageBoxButtons.OK, 
                    MessageBoxIcon.Error);
            }
        }
        
        private static void ApplyModernStyling(Form form)
        {
            // Modern form appearance
            form.BackColor = Color.FromArgb(240, 242, 245);
            form.Font = new Font("Segoe UI", 9F);
            form.Text = "CHART Header Tool";
            form.StartPosition = FormStartPosition.CenterScreen;
            
            // Apply modern styles to all controls recursively
            ApplyModernStylesToControls(form);
        }
        
        private static void ApplyModernStylesToControls(Control parent)
        {
            foreach (Control control in parent.Controls)
            {
                // Style TextBoxes
                if (control is TextBox textBox)
                {
                    textBox.BorderStyle = BorderStyle.FixedSingle;
                    textBox.BackColor = Color.White;
                    textBox.Font = new Font("Segoe UI", 9.5F);
                    textBox.ForeColor = Color.FromArgb(15, 23, 42);
                }
                // Style Labels
                else if (control is Label label)
                {
                    label.Font = new Font("Segoe UI Semibold", 9F);
                    label.ForeColor = Color.FromArgb(51, 65, 85);
                }
                // Style Buttons
                else if (control is Button button)
                {
                    button.FlatStyle = FlatStyle.Flat;
                    button.BackColor = Color.FromArgb(59, 130, 246);
                    button.ForeColor = Color.White;
                    button.Font = new Font("Segoe UI Semibold", 10F);
                    button.FlatAppearance.BorderSize = 0;
                    button.Cursor = Cursors.Hand;
                    
                    // Special colors for specific buttons
                    if (button.Text.Contains("Import") || button.Name == "bImportPrego")
                    {
                        button.BackColor = Color.FromArgb(59, 130, 246); // Blue
                    }
                    else if (button.Text.Contains("Create") || button.Text.Contains("Update") || button.Name == "bCreateUpdate")
                    {
                        button.BackColor = Color.FromArgb(16, 185, 129); // Green
                    }
                    else if (button.Text.Contains("Run") || button.Name == "bRun")
                    {
                        button.BackColor = Color.FromArgb(124, 58, 237); // Purple
                    }
                }
                // Style TabControl
                else if (control is TabControl tabControl)
                {
                    tabControl.Font = new Font("Segoe UI", 10F);
                    foreach (TabPage page in tabControl.TabPages)
                    {
                        page.BackColor = Color.White;
                        page.Font = new Font("Segoe UI", 9F);
                    }
                }
                // Style Panels
                else if (control is Panel panel)
                {
                    if (panel.BorderStyle == BorderStyle.FixedSingle || panel.BorderStyle == BorderStyle.Fixed3D)
                    {
                        panel.BorderStyle = BorderStyle.FixedSingle;
                        panel.BackColor = Color.FromArgb(248, 250, 252);
                    }
                }
                // Style ComboBoxes
                else if (control is ComboBox comboBox)
                {
                    comboBox.FlatStyle = FlatStyle.Flat;
                    comboBox.BackColor = Color.White;
                    comboBox.Font = new Font("Segoe UI", 9.5F);
                }
                // Style CheckBoxes
                else if (control is CheckBox checkBox)
                {
                    checkBox.FlatStyle = FlatStyle.Flat;
                    checkBox.Font = new Font("Segoe UI", 9F);
                    checkBox.ForeColor = Color.FromArgb(51, 65, 85);
                }
                
                // Recursively apply to child controls
                if (control.HasChildren)
                {
                    ApplyModernStylesToControls(control);
                }
            }
        }
    }
}
