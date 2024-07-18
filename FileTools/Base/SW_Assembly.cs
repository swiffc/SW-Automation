using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;

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


        // Protected methods
        protected bool EditDimension(string dimensionName, string treeName, double? newValue)
        {
            if (!newValue.HasValue)
            {
                throw new ArgumentNullException(nameof(newValue));
            }

            bool editSuccessful = false;
            string equationName = $"{dimensionName}@{treeName}";
            Dimension dimension = null;
            try
            {
                dimension = (AssemblyDoc as ModelDoc2).Parameter(equationName);

                if (dimension != null)
                {
                    int message = dimension.SetValue3(newValue.Value, (int)swSetValueInConfiguration_e.swSetValue_UseCurrentSetting, null);

                    if (message == (int)swSetValueReturnStatus_e.swSetValue_Successful)
                        editSuccessful = true;
                    else
                    {
                        swSetValueReturnStatus_e status = (swSetValueReturnStatus_e)message;
                        Debug.WriteLine($"ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR: Dimension {equationName} could not be set. Return status: {status}");
                    }
                }
                else
                    Debug.WriteLine($"ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR: Dimension {equationName} not found.");
            }
            catch (Exception ex)
            {
                Debug.WriteLine($"   Exception encountered: {ex.Message}");
            }
            finally
            {
                if (dimension != null)
                {
                    Marshal.ReleaseComObject(dimension);
                    dimension = null;
                }
            }

            return editSuccessful;
        }
        protected bool[] SuppressSketch(params string[] sketchName)
        {
            ModelDoc2 modelDoc2 = AssemblyDoc as ModelDoc2;

            bool[] results = new bool[sketchName.Length];

            for (int i = 0; i < sketchName.Length; i++)
            {
                string featureName = sketchName[i];
                bool isSelected = modelDoc2.Extension.SelectByID2(featureName, "SKETCH", 0, 0, 0, false, 0, null, 0);

                if (!isSelected)
                {
                    results[i] = false;
                    continue;
                }

                var feature = modelDoc2.ISelectionManager.GetSelectedObject6(1, -1);
                bool isCurrentlySuppressed = ((Feature)feature).IsSuppressed();

                if (!isCurrentlySuppressed)
                {
                    results[i] = modelDoc2.EditSuppress2();
                }
                else
                {
                    results[i] = true;
                }

                modelDoc2.ClearSelection2(true);
            }

            return results;
        }
        protected bool[] UnsuppressSketch(params string[] sketchName)
        {
            ModelDoc2 modelDoc2 = AssemblyDoc as ModelDoc2;

            bool[] results = new bool[sketchName.Length];

            for (int i = 0; i < sketchName.Length; i++)
            {
                string featureName = sketchName[i];
                bool isSelected = modelDoc2.Extension.SelectByID2(featureName, "SKETCH", 0, 0, 0, false, 0, null, 0);

                if (!isSelected)
                {
                    results[i] = false;
                    continue;
                }

                var feature = modelDoc2.ISelectionManager.GetSelectedObject6(1, -1);
                bool isCurrentlySuppressed = ((Feature)feature).IsSuppressed();

                if (isCurrentlySuppressed)
                {
                    results[i] = modelDoc2.EditUnsuppress2();
                }
                else
                {
                    results[i] = true;
                }

                modelDoc2.ClearSelection2(true);
            }

            return results;
        }


        // Virtual methods
        protected virtual void Setup() { }
        protected virtual void Configurations() { }
        protected virtual void Dimensions() { }
        protected virtual void Sketches() { }


        // Public properties
        public AssemblyDoc AssemblyDoc { get; set; }
        public ModelDoc2 ModelDoc2 => AssemblyDoc as ModelDoc2;
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
        public HashSet<string> ProcessedPartNumbers = new HashSet<string>();


        // Static properties
        public static readonly List<Type> ClassesToIsolate = new List<Type>();


        // Private properties
        private Component2[] _components;
    }
}
