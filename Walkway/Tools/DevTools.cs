using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Walkway.Tools
{
    public class DevTools : Walkway
    {
        public static string[] PrintDocumentDependencies(string filePath)
        {
            bool traverseFlag = true;
            bool searchFlag = true;
            bool addReadOnlyInfo = false;

            // Call the GetDocumentDependencies2 method to retrieve the dependencies
            object dependencies = SW.GetDocumentDependencies2(filePath, traverseFlag, searchFlag, addReadOnlyInfo);

            // Convert the object to a string array
            string[] dependenciesArray = dependencies as string[];

            // Check if there are any dependencies and print them
            if (dependenciesArray != null)
            {
                for (int i = 0; i < dependenciesArray.Length; i += 2)
                {
                    string dependencyPath = dependenciesArray[i + 1];
                    Console.WriteLine($"{dependencyPath}");
                }
            }
            else
            {
                Console.WriteLine("No dependencies found.");
            }
            return dependenciesArray;
        }
        private static void PrintPositionMatrixValues()
        {
            ModelDoc2 modelDoc2 = SW.ActiveDoc;
            AssemblyDoc assemblyDoc = modelDoc2 as AssemblyDoc;
            object[] parts = assemblyDoc.GetComponents(false);
            Component2 component2 = parts[0] as Component2;

            for (int i = 0; i < component2.Transform2.ArrayData.Length; i++)
            {
                double x = component2.Transform2.ArrayData[i];
                //if (Math.Abs(x) > 0.01 && Math.Abs(x) < 0.99)
                //{
                Console.WriteLine($"{i}: {x}");
                //}
            }

            Optimize.Release(ref modelDoc2);
            Optimize.Release(ref assemblyDoc);
            Optimize.Release(ref component2);
        }
        public static void InchesToMeters(double[] positionMatrix)
        {
            positionMatrix[9] = positionMatrix[9] * 0.0254;
            positionMatrix[10] = positionMatrix[10] * 0.0254;
            positionMatrix[11] = positionMatrix[11] * 0.0254;
        }
        public static double[] PositionMaxtrix { get; set; } = new double[16]
{
            1,0,0,0,
            1,0,0,0,
            1,0,0,0,
            1,0,0,0
};
        public static void Lock()
        {
            string bankAssembly = $@"{DesktopFolderPath}\{Project}-28{Bank}.SLDASM";

            if (SW.IActiveDoc2 == null && System.IO.File.Exists(bankAssembly))
            {
                Open(bankAssembly);
            }
            else if (SW.IActiveDoc2 == null)
            {
                AddNew_Bank();
            }

            SW.IActiveDoc2.Lock();
        }
        public static void Lock(char bank)
        {
            Bank = bank;
            Lock();
        }
        public static void Unlock()
        {
            SW.IActiveDoc2.UnLock();
        }
        public static void DisablePartUI()
        {
            SW.DocumentVisible(false, (int)swDocumentTypes_e.swDocPART);
        }
        public static void DisableAssemblyUI()
        {
            SW.DocumentVisible(false, (int)swDocumentTypes_e.swDocASSEMBLY);
        }
        public static void EnablePartUI()
        {
            SW.DocumentVisible(true, (int)swDocumentTypes_e.swDocPART);
        }
        public static void EnableAssemblyUI()
        {
            SW.DocumentVisible(true, (int)swDocumentTypes_e.swDocASSEMBLY);
        }
        public static void Rebuild()
        {
            SW.IActiveDoc2.EditRebuild3();
        }
    }
}
