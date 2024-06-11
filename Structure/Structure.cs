using FileTools.Base;
using SolidWorks.Interop.sldworks;
using System.Drawing;
using System;
using System.Windows.Forms;
using static FileTools.StaticFileTools;
using System.Runtime.InteropServices;
using static Plenum.Plenum;
using Structure.Braces.Derived;


namespace Structure
{
    internal class Structure : MainAssembly
    {
        public Structure(int assemblyNumber, string assemblyDescription) : base(assemblyNumber, assemblyDescription 
            //,typeof(BraceL)
            ) { }

        public static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            StructureUI structureUI = new StructureUI();
            Application.Run(structureUI);

            Type[] test = new Type[] { typeof(StructureUI), typeof(Structure) };
        }
    }
}
