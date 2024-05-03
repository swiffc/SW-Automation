using System;
using System.Collections.Generic;

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
        private static readonly Dictionary<string, ComponentEntry> _componentsByPartNo = new Dictionary<string, ComponentEntry>();

        public static void RegisterComponent(IComponentInfo2 component)
        {
            if (component != null && !string.IsNullOrWhiteSpace(component.StaticPartNo))
            {
                // Store both the instance and its type in the dictionary
                _componentsByPartNo[component.StaticPartNo] = new ComponentEntry { Instance = component, ComponentType = component.GetType() };
            }
        }

        // Method to get a component by StaticPartNo
        public static IComponentInfo2 GetComponentByPartNo(string partNo)
        {
            if (_componentsByPartNo.TryGetValue(partNo, out var entry))
            {
                return entry.Instance;
            }

            return null;
        }

        // New method to get a component's type by StaticPartNo
        public static Type GetComponentTypeByPartNo(string partNo)
        {
            if (_componentsByPartNo.TryGetValue(partNo, out var entry))
            {
                return entry.ComponentType;
            }

            return null;
        }
    }
}
