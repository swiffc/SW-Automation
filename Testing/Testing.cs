using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;


namespace Testing
{
    internal class Testing
    {
        private static SldWorks SW = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");
        static void Main()
        {
            if (SW.GetDocuments() != null)
            {
                foreach (ModelDoc2 doc in SW.GetDocuments())
                {
                    Console.WriteLine(doc.GetPathName());
                }
            }
        }
    }
}
