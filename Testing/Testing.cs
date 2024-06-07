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
            SldWorks SW = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");
            string destinationFolderPath = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.Desktop), "Template Export");
            Directory.CreateDirectory(destinationFolderPath);

            foreach (Component2 component in (SW.IActiveDoc2 as AssemblyDoc).GetComponents(false))
            {
                string sourceFilePath = component.GetPathName();
                if (Path.GetExtension(sourceFilePath).ToLower() == ".sldprt")
                {
                    File.Copy(sourceFilePath, Path.Combine(destinationFolderPath, $"JOBNO-{component.ReferencedConfiguration}.sldprt"), true);
                }
            }
        }
    }
}
