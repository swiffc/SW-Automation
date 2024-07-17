using System;
using System.Collections.Generic;
using System.IO;

namespace FileTools.Base
{
    public static class ComponentRegistry
    {
        // Define a structure to hold both instance and Type
        private class ComponentEntry
        {
            public IComponentInfo2 Instance { get; set; }
            public Type ComponentType { get; set; }
        }

        // Modify the dictionary to store ComponentEntry
        private static readonly Dictionary<(string, string), ComponentEntry> _componentsByPartNo = new Dictionary<(string StaticPartNo, string FileName), ComponentEntry>();

        public static void RegisterComponent(IComponentInfo2 component)
        {
            if (component != null && !string.IsNullOrWhiteSpace(component.StaticPartNo))
            {
                // Store both the instance and its type in the dictionary
                string staticPartNo = component.StaticPartNo;
                string fileName = Path.GetFileNameWithoutExtension(component.FilePath);
                _componentsByPartNo[(staticPartNo, fileName)] = new ComponentEntry { Instance = component, ComponentType = component.GetType() };
            }
        }

        // Method to get a component by StaticPartNo
        public static IComponentInfo2 GetComponentByPartNoAndFileName(string partNo, string fileNameWithoutExtension)
        {
            if (_componentsByPartNo.TryGetValue((partNo, fileNameWithoutExtension), out var entry))
            {
                return entry.Instance;
            }

            return null;
        }
        public static IComponentInfo2 GetComponentByPartNo(string partNo)
        {
            foreach (var entry in _componentsByPartNo)
            {
                if (entry.Key.Item1 == partNo)
                {
                    return entry.Value.Instance;
                }
            }

            return null;
        }

        // New method to get a component's type by StaticPartNo
        public static Type GetComponentTypeByPartNo(string partNo, string fileNameWithoutExtension)
        {
            if (_componentsByPartNo.TryGetValue((partNo, fileNameWithoutExtension), out var entry))
            {
                return entry.ComponentType;
            }

            return null;
        }
    }
}
