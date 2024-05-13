using FileTools.Base;
using System;
using System.Windows.Forms;
using static FileTools.CommonData.CommonData;

namespace Bundle
{
    internal class Bundle : MainAssembly
    {
        // Static properties
        static public double Width
        {
            get { return Bundle_Width; }
            set { Bundle_Width = value; }
        }


        // Constructor
        public Bundle(int assemblyNumber, string assemblyDescription) : base(assemblyNumber, assemblyDescription
            //,typeof(Class)
            ) { }


        // Debugging
        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new fBundle());
        }
    }
}
