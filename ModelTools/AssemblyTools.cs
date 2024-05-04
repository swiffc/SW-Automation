using SolidWorks.Interop.sldworks;
using System.Collections.Generic;
using System.Data.Common;
using System.Diagnostics;
using System.IO;
using mTools = Tools.ModelTools;
using cTools = ModelTools.ReleaseCOM;
using System.Runtime.InteropServices;
using System;
using System.ComponentModel;
using System.Reflection;

namespace ModelTools
{
    public static class AssemblyTools
    {
        private static SldWorks SW = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");

        // Public methods
        public static void PlaceComponent(IComponentInfo component, AssemblyDoc assemblyDoc)
        {
            // Search assembly for existing component instances
            List<Component2> componentList = component.PartNo != null
                ? FindMatchingComponents(component.FilePath, assemblyDoc)
                : new List<Component2>();
            bool insertNew = componentList.Count == 0 && component.PartNo != null;
            componentList.Sort((c1, c2) => c1.Name2.CompareTo(c2.Name2));

            // Add to list or remove components from assembly
            while (componentList.Count < component.Position.Count)
            {
                componentList.Add(null);
            }
            while (componentList.Count > component.Position.Count)
            {
                DeleteComponentByName(componentList[componentList.Count - 1].Name2, assemblyDoc);
                componentList.RemoveAt(componentList.Count - 1);
            }

            // Insert or get existing components
            for (int i = 0; i < component.Position.Count; i++)
            {
                componentList[i] = insertNew || componentList[i] == null
                    ? mTools.InsertComponent(component.FilePath, assemblyDoc)
                    : GetInstance(componentList, i);
            }

            // Locate components
            for (int i = 0; i < component.Position.Count; i++)
            {
                if (componentList[i] != null)
                {
                    mTools.X_Translation(component.Position[i].TranslationX);
                    mTools.Y_Translation(component.Position[i].TranslationY);
                    mTools.Z_Translation(component.Position[i].TranslationZ);
                    mTools.Rotate(
                        component.Position[i].RotationX,
                        component.Position[i].RotationY,
                        component.Position[i].RotationZ
                    );
                    mTools.SetPosition(componentList[i]);
                }
            }

            // Memory management
            foreach (var item in componentList)
            {
                cTools.Release(item);
            }
            componentList.Clear();
            mTools.Close(component.FilePath);
        }
        public static Component2[] UnfixedComponentsArray(AssemblyDoc assemblyDoc)
        {
            List<Component2> unfixedComponents = new List<Component2>();

            int componentCount = assemblyDoc.GetComponentCount(true);
            object[] components = (object[])assemblyDoc.GetComponents(true);
            for (int i = 0; i < componentCount; i++)
            {
                var component = (Component2)components[i];

                if (component != null)
                {
                    object[] mates = component.GetMates();

                    if (!component.IsFixed() && mates != null)
                    {
                        unfixedComponents.Add(component);
                    }
                }
            }
            return unfixedComponents.ToArray();
        }
        public static void FixComponentLocations(Component2[] userLocatedComponents, AssemblyDoc assemblyDoc)
        {
            HashSet<Component2> locatedComponentsSet = new HashSet<Component2>(userLocatedComponents);
            List<Component2> componentsToFix = new List<Component2>();

            int componentCount = assemblyDoc.GetComponentCount(true);
            object[] components = (object[])assemblyDoc.GetComponents(true);

            // Collect components that need fixing
            for (int i = 0; i < componentCount; i++)
            {
                var component = (Component2)components[i];
                if (component != null && !component.IsFixed() && !locatedComponentsSet.Contains(component))
                {
                    componentsToFix.Add(component);
                }
            }

            // Select and fix all at once
            if (componentsToFix.Count > 0)
            {
                ModelDoc2 modelDoc = assemblyDoc as ModelDoc2;
                if (modelDoc != null)
                {
                    foreach (var comp in componentsToFix)
                    {
                        comp.Select4(true, null, false);
                    }
                    assemblyDoc.FixComponent();
                    modelDoc.ClearSelection2(true);
                }
            }
        }
        public static string GetPartNoFromAssembly(string staticPartNo, AssemblyDoc assemblyDoc)
        {
            string assemblyDocPath = (assemblyDoc as ModelDoc2).GetPathName();
            Debug.WriteLine(assemblyDocPath);
            string assemblyName = Path.GetFileNameWithoutExtension(assemblyDocPath);

            object[] componentObjs = assemblyDoc.GetComponents(true);
            if (componentObjs != null)
            {
                foreach (var componentObj in componentObjs)
                {
                    Component2 component = componentObj as Component2;
                    string config = component.ReferencedConfiguration;

                    if (config == staticPartNo)
                    {
                        string partName = Path.GetFileNameWithoutExtension(component.GetPathName());
                        string partNo = ExtractPartNo(component);
                        Debug.WriteLine($"Found part {partName} in assembly {assemblyName}");
                        return partNo;
                    }
                }
            }

            return null;
        }
        public static string GetPartNoFromDirectory(string staticPartNo, AssemblyDoc assemblyDoc)
        {
            ModelDoc2 modelDoc = assemblyDoc as ModelDoc2;
            string parentPath = modelDoc.GetPathName();
            string parentName = Path.GetFileNameWithoutExtension(parentPath);
            Debug.WriteLine($"   Looking for part {staticPartNo} in {parentName}'s directory...");

            // Components in directory
            List<string> directoryComponentPaths = new List<string>();
            string assemblyDirectory = Path.GetDirectoryName(parentPath);
            string[] filePaths = Directory.GetFiles(assemblyDirectory);

            foreach (string filePath in filePaths)
            {
                string extension = Path.GetExtension(filePath).ToLower();
                string fileName = Path.GetFileName(filePath);

                // Check if the file is not temporary and has the right extension
                if (!fileName.StartsWith("~$") && (extension == ".sldprt" || extension == ".sldasm"))
                {
                    directoryComponentPaths.Add(filePath);
                    //Console.WriteLine(" " + filePath);
                }
            }


            // Components loaded into memory
            HashSet<string> loadedComponentPaths = new HashSet<string>
            {
                parentPath
            };
            object[] componentObjs = assemblyDoc.GetComponents(false);
            if (componentObjs != null)
            {
                foreach (var componentObj in componentObjs)
                {
                    Component2 component = componentObj as Component2;
                    loadedComponentPaths.Add(component.GetPathName());
                    //Console.WriteLine("  " + component.GetPathName());

                    cTools.Release(ref component);
                }
            }

            // Components not loaded into memory
            List<string> unloadedComponentPaths = new List<string>();
            foreach (string directoryComponentPath in directoryComponentPaths)
            {
                if (!loadedComponentPaths.Contains(directoryComponentPath))
                {
                    unloadedComponentPaths.Add(directoryComponentPath);
                }
            }

            // Load unloaded components
            mTools.DisablePartUI();
            string partNo = null;
            foreach (string unloadedComponentPath in unloadedComponentPaths)
            {
                ModelDoc2 temp = mTools.Open(unloadedComponentPath);
                object[] configsArray = temp.GetConfigurationNames();
                foreach (var configObj in configsArray)
                {
                    string configName = configObj as string;
                    Debug.WriteLine("   " + configName);
                    if (configName == staticPartNo)
                    {
                        Debug.WriteLine("Found existing!");
                        partNo = ExtractPartNoFromPath(unloadedComponentPath);
                        //mTools.Close(unloadedComponentPath);
                        break;
                    }
                }
                cTools.Release(ref temp);
            }
            mTools.EnablePartUI();

            return partNo;
        }
        public static bool DeleteComponentByName(string compName, AssemblyDoc assemblyDoc)
        {
            ModelDoc2 modelDoc2 = assemblyDoc as ModelDoc2;
            string assemblyPartNumber = Path.GetFileNameWithoutExtension(modelDoc2.GetPathName());

            bool isSelected = modelDoc2.Extension.SelectByID2($"{compName}@{assemblyPartNumber}", "COMPONENT", 0, 0, 0, false, 0, null, 0);
            if (isSelected)
            {
                modelDoc2.Extension.DeleteSelection2(0);
            }
            return isSelected;
        }
        public static string ExtractPartNo(Component2 component2)
        {
            string pathName = component2.GetPathName();
            return ExtractPartNoFromPath(pathName);
        }


        public static List<Component2> FindMatchingComponents(string filePath_OfTarget, AssemblyDoc assemblyDoc_OfSearchLocation)
        {
            // List to store matching components
            List<Component2> matchingComponents = new List<Component2>();

            // Extract the file name from the file path
            string[] fileNameParts = Path.GetFileNameWithoutExtension(filePath_OfTarget).Split('.');

            string fileName = fileNameParts[0];

            // Get all components in the assembly
            object[] allComponentsArray = assemblyDoc_OfSearchLocation.GetComponents(false);

            // Iterate through all components
            if (allComponentsArray != null)
            {
                foreach (var componentObj in allComponentsArray)
                {
                    Component2 component = componentObj as Component2;
                    string componentName = component.Name2;

                    // Check if the component name contains the file name and does not contain "/"
                    if (componentName.Contains(fileName) && !componentName.Contains("/"))
                    {
                        matchingComponents.Add(component);
                    }
                }
            }

            return matchingComponents;
        }
        public static Component2 GetInstance(List<Component2> componentList, int instance)
        {
            Component2 swComponent = componentList[instance];
            Debug.WriteLine($"Existing component found: {swComponent.Name2} at instance {instance}");
            return swComponent;
        }


        public static string ExtractPartNo(string filePath)
        {
            return ExtractPartNoFromPath(filePath);
        }

        public static string ExtractPartNoFromPath(string path)
        {
            string fileName = Path.GetFileNameWithoutExtension(path);

            // Find the last occurrence of '-' or '_'
            int lastDash = fileName.LastIndexOf('-');
            int lastUnderscore = fileName.LastIndexOf('_');

            // Determine which of the two delimiters comes last
            int lastDelimiter = Math.Max(lastDash, lastUnderscore);

            // If a delimiter was found, extract the substring after it; otherwise, use the entire input
            string result = lastDelimiter >= 0 ? fileName.Substring(lastDelimiter + 1) : fileName;

            return result;
        }
    }
}
