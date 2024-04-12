using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace Walkway.Tools
{
    internal class Optimize : Walkway
    {
        public static void DisablePartUI()
        {
            //SW.DocumentVisible(false, (int)swDocumentTypes_e.swDocPART);
        }
        public static void DisableAssemblyUI()
        {
            //SW.DocumentVisible(false, (int)swDocumentTypes_e.swDocASSEMBLY);
        }
        public static void EnablePartUI()
        {
            SW.DocumentVisible(true, (int)swDocumentTypes_e.swDocPART);
        }
        public static void EnableAssemblyUI()
        {
            SW.DocumentVisible(true, (int)swDocumentTypes_e.swDocASSEMBLY);
        }
        private static void GarbageCollection()
        {
            GC.Collect();
            GC.WaitForPendingFinalizers();
            GC.Collect();
            GC.WaitForPendingFinalizers();
            Debug.WriteLine("\n");
        }
        public static void Release(ref ModelDoc2 modelDoc2)
        {
            if (modelDoc2 != null)
            {
                Marshal.ReleaseComObject(modelDoc2);
                modelDoc2 = null;
            }
        }
        public static void Release(ref DrawingDoc drawingDoc)
        {
            if (drawingDoc != null)
            {
                Marshal.ReleaseComObject(drawingDoc);
                drawingDoc = null;
            }
        }
        public static void Release(ref AssemblyDoc assemblyDoc)
        {
            if (assemblyDoc != null)
            {
                string docName = SW.IActiveDoc2.GetTitle();
                string[] partNumber = docName.Split('-');
                Marshal.ReleaseComObject(assemblyDoc);
                assemblyDoc = null;
            }
        }
        public static void Release(ref Component2 component2)
        {
            if (component2 != null)
            {
                int refCount = Marshal.ReleaseComObject(component2);
                if (refCount == 0)
                {
                    component2 = null;
                }
            }
        }
        public static void Release(ref Dimension dimension)
        {
            if (dimension != null)
            {
                int refCount = 0;
                do
                {
                    string dimName = dimension.Name;
                    refCount = Marshal.ReleaseComObject(dimension);
                }
                while (refCount > 0);

                dimension = null;
            }
        }
        public static void Release(ref MathTransform mathTransform)
        {
            if (mathTransform != null)
            {
                int refCount = 0;
                do
                {
                    refCount = Marshal.ReleaseComObject(mathTransform);
                }
                while (refCount > 0);

                mathTransform = null;
            }
        }
        public static void Release(ref IView view)
        {
            if (view != null)
            {
                int refCount = 0;
                do
                {
                    refCount = Marshal.ReleaseComObject(view);
                }
                while (refCount > 0);

                view = null;
            }
        }
    }
}
