using FileTools.Base;
using MachineryMount;
using System;
using System.Windows.Forms;
using static FileTools.CommonData.CommonData;

namespace MachineryMount
{
    internal class MachineryMount : MainAssembly
    {
        // Constructor
        public MachineryMount(int assemblyNumber, string assemblyDescription) : base(assemblyNumber, assemblyDescription
            //,typeof(Class)
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
