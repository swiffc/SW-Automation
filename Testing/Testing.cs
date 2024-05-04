using SolidWorks.Interop.sldworks;
using System;
using System.Runtime.InteropServices;


namespace Testing
{
    internal class Testing
    {
        static void Main()
        {
            SldWorks SW = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");
            ModelDoc2 modelDoc2 = SW.ActiveDoc;
            var selectionMgr = modelDoc2.SelectionManager;
            Component2 component = selectionMgr.GetSelectedObject6(1, -1) as Component2;

            for (int i = 0; i < component.Transform2.ArrayData.Length; i++)
            {
                double x = component.Transform2.ArrayData[i];
                //if (Math.Abs(x) > 0.01 && Math.Abs(x) < 0.99)
                //{
                Console.WriteLine($"{i}: {x}");
                //}
            }
        }

        static void Method()
        {
            SldWorks SW = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");
            ModelDoc2 modelDoc2 = SW.IActiveDoc2;
            var selectionMgr = modelDoc2.SelectionManager;
            Component2 component = selectionMgr.GetSelectedObject6(1, -1) as Component2;

            MathTransform mathTransform = component.Transform2;
            double[] arrayData = mathTransform.ArrayData;
            foreach (var data in arrayData)
            {
                Console.WriteLine(data);
            }
        }
    }
}
