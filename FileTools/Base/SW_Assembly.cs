using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Linq;

namespace FileTools.Base
{
    public class SW_Assembly
    {
        // Public methods
        public void ClearComponentArray()
        {
            _components = null;
            var componentArray = ComponentArray;
        }


        // Public properties
        public AssemblyDoc AssemblyDoc { get; set; }
        public Component2[] ComponentArray
        {
            get
            {
                if (_components == null)
                {
                    object[] componentObjs = AssemblyDoc.GetComponents(true);
                    if (componentObjs != null)
                    {
                        _components = componentObjs.OfType<Component2>().ToArray();
                    }
                    else
                    {
                        _components = Array.Empty<Component2>();
                    }
                }
                return _components;
            }
        }
        public List<IComponentInfo2> GrandChildren = new List<IComponentInfo2>();
        public string Config
        {
            get
            {
                return (AssemblyDoc as ModelDoc2).ConfigurationManager.ActiveConfiguration.Name;
            }
        }


        // Static properties
        public static readonly List<Type> ClassesToIsolate = new List<Type>();


        // Private properties
        private Component2[] _components;
    }
}
