using FileTools.Base;
using SolidWorks.Interop.sldworks;
using System.Drawing;
using System;
using System.Windows.Forms;
using static FileTools.StaticFileTools;
using System.Runtime.InteropServices;


namespace Structure
{
    internal class Structure : MainAssembly
    {
        public Structure(int assemblyNumber, string assemblyDescription) : base(assemblyNumber, assemblyDescription) { }

        public static void Main() // Compiler use only
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            StructureUI structureUI = new StructureUI();
            Application.Run(structureUI);
        }
    }
}
