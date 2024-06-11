using SolidWorks.Interop.sldworks;
using System;
using System.IO;
using System.Runtime.InteropServices;


namespace Testing
{
    internal class Testing
    {
        private static SldWorks SW = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");
        static void Main()
        {
            string desktopPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.Desktop);
            string username = new DirectoryInfo(desktopPath).Parent.Name;
            string initials = username.Substring(0, 3).ToUpper();
            Console.WriteLine(initials);
        }
    }
}
