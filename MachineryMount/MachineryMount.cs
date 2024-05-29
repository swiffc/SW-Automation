using FileTools.Base;
using MachineryMount;
using MachineryMount.DriveWeldment.Children;
using MachineryMount.DriveWeldment.Children.Derived;
using System;
using System.Windows.Forms;
using static FileTools.CommonData.CommonData;

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
            ,typeof(Stringer)
            )
        { }


        // Debugging
        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new MachineryMountUI());
        }
    }
}
