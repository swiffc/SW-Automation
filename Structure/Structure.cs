using FileTools.Base;
using SolidWorks.Interop.sldworks;
using System.Drawing;
using System;
using System.Windows.Forms;
using static FileTools.StaticFileTools;
using System.Runtime.InteropServices;
<<<<<<< HEAD
=======
using static Plenum.Plenum;
>>>>>>> releases/v4.0.0


namespace Structure
{
    internal class Structure : MainAssembly
    {
        public Structure(int assemblyNumber, string assemblyDescription) : base(assemblyNumber, assemblyDescription) { }

<<<<<<< HEAD
        public static void Main() // Compiler use only
=======
        public static void Main()
>>>>>>> releases/v4.0.0
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            StructureUI structureUI = new StructureUI();
            Application.Run(structureUI);
        }
    }
}
