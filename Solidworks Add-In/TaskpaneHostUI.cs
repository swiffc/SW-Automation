using Automation_Library;
using SolidWorks.Interop.sldworks;
using System;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace SolidWorks_Add_In
{
    [ProgId(TaskpaneIntegration.SWTASKPANE_PROGID)]
    public partial class TaskpaneHostUI : UserControl
    {
        public TaskpaneHostUI()
        {
            InitializeComponent();
        }

        private void AlignDimensions_Click(object sender, System.EventArgs e)
        {
            Dimensions dimensions = new Dimensions();
            dimensions.AlignDimensions();
        }

        private void DeleteDanglingDimensions_Click(object sender, System.EventArgs e)
        {
            Dimensions dimensions = new Dimensions();
            dimensions.DeleteDanglingAnnotations();
        }
    }
}
