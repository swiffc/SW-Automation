using ModelTools;
using Plenum.StandardParts;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Threading;
using static Plenum.Plenum;
using aTools = ModelTools.AssemblyTools;
using cTools = ModelTools.ReleaseCOM;
using mTools = Tools.ModelTools;

namespace Plenum
{
    internal abstract class Assembly : IComponentInfo
    {
        // Constructor
        protected Assembly(CallerType callerType)
        {
            CallerType = callerType;
            AssemblyDoc = FTools.OpenAssembly(FilePath, StaticPartNo, true);
            RemoveUnneededSubComponents(AssemblyDoc);

            Component2[] userLocatedComponents = aTools.UnfixedComponentsArray(AssemblyDoc);

            InstantiateSubComponents(AssemblyDoc);
            PlaceSubComponents(AssemblyDoc);
            aTools.FixComponentLocations(userLocatedComponents, AssemblyDoc);

            cTools.Release(AssemblyDoc);
        }


        // Virtual methods
        protected virtual void InstantiateSubComponents(AssemblyDoc assemblyDoc) { }
        protected virtual void PlaceSubComponents(AssemblyDoc assemblyDoc) { }


        // Static methods
        internal static void RemoveUnneededSubComponents(AssemblyDoc assemblyDoc)
        {
            var componentObjs = CollectComponents(assemblyDoc);
            if (componentObjs != null)
            {
                var subComponentStaticNumbers = BuildStaticNumberDictionary(componentObjs);
                var subComponentTypes = DetermineComponentTypes(subComponentStaticNumbers);
                var componentsToDelete = IdentifyComponentsToDelete(subComponentTypes);

                RemoveComponents(componentsToDelete, assemblyDoc);
            }
        }


        // Private methods
        private static object[] CollectComponents(AssemblyDoc assemblyDoc)
        {
            return assemblyDoc.GetComponents(true);
        }
        private static Dictionary<Component2, string> BuildStaticNumberDictionary(object[] componentObjs)
        {
            var dictionary = new Dictionary<Component2, string>();

            foreach (var componentObj in componentObjs)
            {
                Component2 component = componentObj as Component2;
                if (component != null)
                {
                    string staticPartNo = component.ReferencedConfiguration;
                    dictionary[component] = staticPartNo;
                }
            }

            return dictionary;
        }
        private static Dictionary<Component2, Type> DetermineComponentTypes(Dictionary<Component2, string> staticNumbers)
        {
            var dictionary = new Dictionary<Component2, Type>();

            foreach (var component in staticNumbers.Keys)
            {
                string staticNumber = staticNumbers[component];
                Type componentType = null;

                if (StaticParentDictionary.TryGetValue(staticNumber, out componentType) ||
                    StaticChildDictionary.TryGetValue(staticNumber, out componentType))
                {
                    dictionary[component] = componentType;
                }
                else
                {
                    Debug.WriteLine($"Static number {staticNumber} not found for component.");
                }

                double? key = FindKeyByValue(FanGuard.StaticFanGuardDictionary, staticNumber);
                if (key.HasValue)
                {
                    dictionary[component] = typeof(FanGuard);
                }


            }

            return dictionary;
        }
        private static List<Component2> IdentifyComponentsToDelete(Dictionary<Component2, Type> componentTypes)
        {
            var componentsToDelete = new List<Component2>();

            foreach (var kvp in componentTypes)
            {
                Component2 component = kvp.Key;
                Type componentType = kvp.Value;
                PropertyInfo enabledProperty = componentType.GetProperty("Enabled", BindingFlags.Public | BindingFlags.Static);

                if (enabledProperty != null)
                {
                    bool isEnabled = (bool)enabledProperty.GetValue(null);
                    if (!isEnabled)
                    {
                        componentsToDelete.Add(component);
                    }
                }
                else
                {
                    Debug.WriteLine($"Type {componentType.Name} does not contain a public static 'Enabled' property.");
                }
            }

            return componentsToDelete;
        }
        private static void RemoveComponents(List<Component2> componentsToDelete, AssemblyDoc assemblyDoc)
        {
            foreach (var component in componentsToDelete)
            {
                string filePath = component.GetPathName();
                mTools.RemoveComponent(component, assemblyDoc);
                cTools.Release(component);

                if (!mTools.ToBeDeleted.Contains(filePath))
                    mTools.ToBeDeleted.Add(filePath);
            }
        }
        private static double? FindKeyByValue(Dictionary<double, string> dictionary, string value)
        {
            foreach (var pair in dictionary)
            {
                if (pair.Value == value)
                {
                    return pair.Key; // Return the first key that matches the value
                }
            }
            return null; // Return null if the value is not found
        }



        // Abstract properties
        public abstract string StaticPartNo { get; }
        protected abstract AssemblyDoc ParentAssembly { get; }
        public abstract List<PositionData> Position { get; }


        // Non-abstract properties
        public string FilePath
        {
            get
            {
                if (_filePath == null)
                {
                    var partNo = PartNo;

                    _filePath = FTools.GetFilePath(PartNo, FileType);
                }
                return _filePath;
            }
        }
        public string PartNo
        {
            get
            {
                if (_partNo == null)
                {
                    _partNo = FTools.GetPartNoFromAssembly(StaticPartNo, ParentAssembly)
                             ?? aTools.GetPartNoFromDirectory(StaticPartNo, Plenum.AssemblyDoc)
                             ?? FTools.CreateNew_ComponentFile(StaticPartNo);
                }
                return _partNo;
            }
        }
        protected static CallerType CallerType { get; set; }
        protected AssemblyDoc AssemblyDoc { get; set; }


        // Backing fields
        private string _filePath;
        private string _partNo;
        protected List<PositionData> _position;


        // Constants
        private const string FileType = "SLDASM";
    }
}
