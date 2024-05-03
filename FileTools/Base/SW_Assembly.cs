using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

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


        // Private properties
        private Component2[] _components;
    }
}
