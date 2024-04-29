using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using static FileTools.Properties.Settings;

namespace SolidWorks_Add_In
{
    public partial class DevUI : Form
    {
        public DevUI()
        {
            InitializeComponent();
        }

        private void button_ResetDefaults_Click(object sender, EventArgs e)
        {
            Default.Reset();
        }
    }
}
