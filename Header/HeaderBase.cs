using FileTools.Base;
using System;
using System.Collections.Generic;
using System.Windows.Forms;
using static FileTools.CommonData.CommonData;
using static Excel.Prego;
using HDR.Box;
using HDR.Box.Derived;
namespace HDR
{
    internal class HeaderBase : MainAssembly
    {
        // Static properties
        public static IHeaderExtensions Header { get; set; }
        static public double ModelLengthReduction => 6;


        // Constructor
        public HeaderBase(int assemblyNumber, string assemblyDescription) : base(assemblyNumber, assemblyDescription
            //, typeof(EndPlate2)
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
