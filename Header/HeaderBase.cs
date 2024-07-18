using FileTools.Base;
using System;
using System.Collections.Generic;
using System.Windows.Forms;
using static FileTools.CommonData.CommonData;
using static Excel.Prego;
using HDR.Box;
using HDR.Box.Derived;
using HDR.Connections;

namespace HDR
{
    internal class HeaderBase : MainAssembly
    {
        // Static properties
        public static IHeaderExtensions Header { get; set; }
        static public double ModelLengthReduction => 6;
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
