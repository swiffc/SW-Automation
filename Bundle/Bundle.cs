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
            )
        { }


        // Method overrides
        protected override void Dimensions()
        {
            if (Header61.IsRequired)
            {
                EditDimension("Length", "61", );
                EditDimension("Depth", "61", Header61.BoxWidth + Header61.TubesheetTHK + Header61.PlugsheetTHK);
                EditDimension("Height", "61", );
                EditDimension("Y", "61", );
                EditDimension("Z", "61", );
            }
            if (Header62.IsRequired)
            {
                EditDimension("Length", "62", );
                EditDimension("Depth", "62", Header62.BoxWidth + Header62.TubesheetTHK + Header62.PlugsheetTHK);
                EditDimension("Height", "62", );
                EditDimension("Y", "62", );
                EditDimension("Z", "62", );
            }
            if (Header63.IsRequired)
            {
                EditDimension("Length", "63", );
                EditDimension("Depth", "63", Header63.BoxWidth + Header63.TubesheetTHK + Header63.PlugsheetTHK);
                EditDimension("Height", "63", );
                EditDimension("Y", "63", );
                EditDimension("Z", "63", );
            }
            if (Header64.IsRequired)
            {
                EditDimension("Length", "64", );
                EditDimension("Depth", "64", Header64.BoxWidth + Header64.TubesheetTHK + Header64.PlugsheetTHK);
                EditDimension("Height", "64", );
                EditDimension("Y", "64", );
                EditDimension("Z", "64", );
            }
            if (Header65.IsRequired)
            {
                EditDimension("Length", "65", );
                EditDimension("Depth", "65", Header65.BoxWidth + Header65.TubesheetTHK + Header65.PlugsheetTHK);
                EditDimension("Height", "65", );
                EditDimension("Y", "65", );
                EditDimension("Z", "65", );
            }
            if (Header66.IsRequired)
            {
                EditDimension("Length", "66", );
                EditDimension("Depth", "66", Header66.BoxWidth + Header66.TubesheetTHK + Header66.PlugsheetTHK);
                EditDimension("Height", "66", );
                EditDimension("Y", "66", );
                EditDimension("Z", "66", );
            }
        }
        protected override void Sketches()
        {
            
        }


        // Debugging
        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new BundleUI());
        }
    }
}
