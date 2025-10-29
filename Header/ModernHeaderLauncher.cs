using System;
using System.Windows.Forms;

namespace HDR
{
    class ModernHeaderLauncher
    {
        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new ModernHeaderUI());
        }
    }
}
