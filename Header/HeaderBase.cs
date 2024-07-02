using FileTools.Base;
using System;
using System.Windows.Forms;
using static FileTools.CommonData.CommonData;
namespace HDR
{
    internal class HeaderBase : MainAssembly
    {
        // Static properties
        public static IHeaderExtensions Header { get; set; }
        static public double ModelLengthReduction => 4;


        // Constructor
        public HeaderBase(int assemblyNumber, string assemblyDescription) : base(assemblyNumber, assemblyDescription
            //, typeof(Type)
            )
        { }


        // Debugging
        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new HeaderUI());
        }
    }
}
