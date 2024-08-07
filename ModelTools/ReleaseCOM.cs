using EPDM.Interop.epdm;
using SolidWorks.Interop.sldworks;
using System.Diagnostics;
using System;
using System.Runtime.InteropServices;
using mTools = Tools.ModelTools;

namespace ModelTools
{
    public class ReleaseCOM
    {

        public static void Release(ref SketchPoint COM)
        {
            if (COM != null)
            {
                Marshal.ReleaseComObject(COM);
                COM = null;
                //Debug.WriteLine("COM resource released");
            }
        }
        public static void Release(ref SketchSegment COM)
        {
            if (COM != null)
            {
                Marshal.ReleaseComObject(COM);
                COM = null;
                //Debug.WriteLine("COM resource released");
            }
        }
        public static void Release(ref Feature COM)
        {
            if (COM != null)
            {
                Marshal.ReleaseComObject(COM);
                COM = null;
                //Debug.WriteLine("COM resource released");
            }
        }
        public static void Release(ref IModelDocExtension COM)
        {
            if (COM != null)
            {
                Marshal.ReleaseComObject(COM);
                COM = null;
                //Debug.WriteLine("COM resource released");
            }
        }
        public static void Release(ref FeatureManager COM)
        {
            if (COM != null)
            {
                Marshal.ReleaseComObject(COM);
                COM = null;
                //Debug.WriteLine("COM resource released");
            }
        }
        public static void Release(ref ModelDoc2 COM)
        {
            if (COM != null)
            {
                Marshal.ReleaseComObject(COM);
                COM = null;
                //Debug.WriteLine("COM resource released");
            }
        }
        public static void Release(ref Component2 COM)
        {
            if (COM != null)
            {
                Marshal.ReleaseComObject(COM);
                COM = null;
                //Debug.WriteLine("COM resource released");
            }
        }
        public static void ReleaseUnload(ref Component2 COM)
        {
            mTools.Close(COM.GetPathName());
            if (COM != null)
            {
                Marshal.ReleaseComObject(COM);
                COM = null;
                //Debug.WriteLine("COM resource released");
            }
        }

        public static void Release(ref EdmVault5 COM)
        {
            if (COM != null)
            {
                Marshal.ReleaseComObject(COM);
                COM = null;
                //Debug.WriteLine("COM resource released");
            }
        }
        public static void Release(ref IEdmFolder5 COM)
        {
            if (COM != null)
            {
                Marshal.ReleaseComObject(COM);
                COM = null;
                //Debug.WriteLine("COM resource released");
            }
        }
        public static void Release(ref IEdmFile5 COM)
        {
            if (COM != null)
            {
                Marshal.ReleaseComObject(COM);
                COM = null;
                //Debug.WriteLine("COM resource released");
            }
        }
        public static void Release(ref MathTransform COM)
        {
            if (COM != null)
            {
                Marshal.ReleaseComObject(COM);
                COM = null;
                //Debug.WriteLine("COM resource released");
            }
        }
        public static void Release(ref AssemblyDoc COM)
        {
            if (COM != null)
            {
                Marshal.ReleaseComObject(COM);
                COM = null;
                //Debug.WriteLine("COM resource released");
            }
        }
        public static void ReleaseUnload(ref AssemblyDoc COM)
        {
            mTools.Close((COM as ModelDoc2).GetPathName());
            if (COM != null)
            {
                Marshal.ReleaseComObject(COM);
                COM = null;
                //Debug.WriteLine("COM resource released");
            }
        }
        public static void Release(ref SelectionMgr COM)
        {
            if (COM != null)
            {
                Marshal.ReleaseComObject(COM);
                COM = null;
                //Debug.WriteLine("COM resource released");
            }
        }
        public static void Release(ref DrawingDoc COM)
        {
            if (COM != null)
            {
                Marshal.ReleaseComObject(COM);
                COM = null;
                //Debug.WriteLine("COM resource released");
            }
        }
        
        public static void Release(ref CustomPropertyManager COM)
        {
            if (COM != null)
            {
                Marshal.ReleaseComObject(COM);
                COM = null;
                //Debug.WriteLine("COM resource released");
            }
        }






        public static void Release(SketchPoint COM)
        {
            if (COM != null)
            {
                Marshal.ReleaseComObject(COM);
                COM = null;
                //Debug.WriteLine("COM resource released");
            }
        }
        public static void Release(Component2 COM)
        {
            if (COM != null)
            {
                Marshal.ReleaseComObject(COM);
                COM = null;
                //Debug.WriteLine("COM resource released");
            }
        }
        public static void Release(AssemblyDoc COM)
        {
            if (COM != null)
            {
                Marshal.ReleaseComObject(COM);
                COM = null;
                //Debug.WriteLine("COM resource released");
            }
        }
        public static void Release(ModelDoc2 COM)
        {
            if (COM != null)
            {
                Marshal.ReleaseComObject(COM);
                COM = null;
                //Debug.WriteLine("COM resource released");
            }
        }
    }
}
