using FileTools.Base;
using System;
using System.Collections.Generic;
using System.Windows.Forms;
using static FileTools.CommonData.CommonData;
using static Excel.Prego;
namespace HDR
{
    internal class HeaderBase : MainAssembly
    {
        // Static properties
        public static IHeaderExtensions Header { get; set; }
        static public double ModelLengthReduction => 6;


        // Constructor
        public HeaderBase(int assemblyNumber, string assemblyDescription) : base(assemblyNumber, assemblyDescription
            //, typeof(Type)
            )
        { }


        // ???
        internal static List<double> Bust_63
        {
            get
            {
                var bustedSpans_63 = CellNameColumnArray("AAJ5", "AAJ43");
                var list_63 = CellDoubleList(InputsCalcsSheet, bustedSpans_63);
                return list_63;
            }
        }


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
