using FileTools.Base;
using MachineryMount.BeltGuard.Children;
using MachineryMount.DriveWeldment.Children;
using System;
using System.Windows.Forms;
using static FileTools.CommonData.CommonData;
using MachineryMount.BeltGuard;
using MachineryMount.DriveWeldment.Children.Derived;
using MachineryMount.DriveWeldment;
using MachineryMount.DriveAssembly;
using MachineryMount.MotorMount;
using MachineryMount.MotorMount.Children;
using AXC_Vault;
using static FileTools.StaticFileTools;
using System.Runtime.InteropServices;
using System.Threading;
using static MachineryMount.MachineryMount;

namespace MachineryMount
{
    internal class MachineryMount : MainAssembly
    {
        // Static properties
        public static double CenterToCenter
        {
            get { return MotorCenter_To_FanCenter; }
            set { MotorCenter_To_FanCenter = value; }
        }


        // Constructor
        public MachineryMount(int assemblyNumber, string assemblyDescription) : base(assemblyNumber, assemblyDescription
            //, typeof(MotorMountWeldment)
            )
        {
            Stringer._flangeWidth = null;
            Stringer._webTHK = null;
        }


        // Protected methods
        protected override void Setup()
        {
            if (!Developer)
            {
                var thread = new Thread(() =>
                {
                    _messageBoxForm = new MessageBoxForm();
                    Application.Run(_messageBoxForm);
                });
                thread.SetApartmentState(ApartmentState.STA);
                thread.Start();

                if (!TemplatesDownloaded)
                {
                    Vault.GetAllFilesInFolder(@"C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting\Certified\MachineryMount\");
                    TemplatesDownloaded = true;
                }
                if (!LibraryFilesDownloaded)
                {
                    Vault.GetAllFilesInFolder(@"C:\AXC_VAULT\SolidWorks Common Data\Design Library\Hudson Library\Bearings\");
                    LibraryFilesDownloaded = true;
                }

                _messageBoxForm.Invoke(new Action(_messageBoxForm.Close));
            }
        }


        // Debugging
        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new MachineryMountUI());
        }


        // Form
        private MessageBoxForm _messageBoxForm;
        public class MessageBoxForm : Form
        {
            public MessageBoxForm()
            {
                Load += (sender, e) => ShowMessageBox();
            }

            private void ShowMessageBox()
            {
                MessageBox.Show("Please wait...", "AXC_VAULT", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
        }
    }
}
