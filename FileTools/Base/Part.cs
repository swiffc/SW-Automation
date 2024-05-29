using ModelTools;
using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.InteropServices;
using static FileTools.StaticFileTools;
using static ModelTools.ReleaseCOM;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;
using System.IO;

namespace FileTools.Base
{
    public abstract class Part : IComponentInfo2
    {
        // Constructors
        protected Part(SubAssembly parentSubAssembly)
        {
            if (Enabled)
            {
                _parentAssembly = parentSubAssembly;
                ParentSubAssembly = parentSubAssembly;
                InitializePart();
            }
        }
        protected Part(SW_Assembly parentMainAssembly)
        {
            if (Enabled)
            {
                _parentAssembly = parentMainAssembly;
                InitializePart();
            }
        }


        // Private methods
        private void InitializePart()
        {
            ModelDoc2 = OpenSilent(this);
            Dimensions();
            Features();
            Release(ModelDoc2);
        }


        // Protected methods
        protected bool EditDimension(string dimensionName, string treeName, double newValue)
        {
            bool editSuccessful = false;
            string equationName = $"{dimensionName}@{treeName}";
            Dimension dimension = null;
            try
            {
                dimension = ModelDoc2.Parameter(equationName);

                if (dimension != null)
                {
                    int message = dimension.SetValue3(newValue, (int)swSetValueInConfiguration_e.swSetValue_UseCurrentSetting, null);

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
        protected bool EditFeature_StructuralMemberSize(string newSize)
        {
            IStructuralMemberFeatureData member = null;

            // Iterate through features to find the first structural member
            Feature feature = ModelDoc2.FirstFeature();
            while (feature != null)
            {
                member = feature.GetDefinition() as IStructuralMemberFeatureData;

                if (member != null) break;

                feature = feature.GetNextFeature();
            }

            if (member != null)
            {
                string previousSize = member.ConfigurationName;
                if (previousSize != newSize)
                {
                    ActivateModelDoc(ModelDoc2);
                    bool selectionsAccessed = member.AccessSelections(ModelDoc2, null);
                    member.ConfigurationName = newSize;
                    bool definitionModified = feature.ModifyDefinition(member, ModelDoc2, null);
                    ModelDoc2.Visible = false;

                    // Turn off sketch
                    double number = 0;
                    string sketchName;
                    bool isSelected = false;
                    while (!isSelected)
                    {
                        number++;
                        sketchName = "Sketch" + number;
                        isSelected = ModelDoc2.Extension.SelectByID2(sketchName, "SKETCH", 0, 0, 0, false, 0, null, 0);
                    }
                    ModelDoc2.BlankSketch();

                    return definitionModified;
                }
                return true;
            }
            else return false;

        }
        protected void EditFeature_StructuralMemberSize(string newSize, out double flangeWidth)
        {
            flangeWidth = 0;
            IStructuralMemberFeatureData member = null;

            // Iterate through features to find the first structural member
            Feature feature = ModelDoc2.FirstFeature();
            while (feature != null)
            {
                member = feature.GetDefinition() as IStructuralMemberFeatureData;

                if (member != null) break;

                feature = feature.GetNextFeature();
            }

            if (member != null)
            {
                string previousSize = member.ConfigurationName;
                if (previousSize != newSize)
                {
                    ActivateModelDoc(ModelDoc2);
                    bool selectionsAccessed = member.AccessSelections(ModelDoc2, null);
                    member.ConfigurationName = newSize;
                    bool definitionModified = feature.ModifyDefinition(member, ModelDoc2, null);
                    //ModelDoc2.Visible = false;

                    // Find sketch
                    double number = 0;
                    string sketchName;
                    bool isSelected = false;


                    while (!isSelected)
                    {
                        number++;
                        sketchName = "Sketch" + number;
                        string fileName = Path.GetFileName(ModelDoc2.GetPathName());
                        string dimensionName = $"BF@{sketchName}@{fileName}";
                        isSelected = ModelDoc2.Extension.SelectByID2(dimensionName, "DIMENSION", 0, 0, 0, false, 0, null, 0);

                        ISelectionMgr swSelMgr = (ISelectionMgr)ModelDoc2.SelectionManager;
                        IDimension swDimension = (IDimension)swSelMgr.GetSelectedObject6(1, -1);
                        flangeWidth = swDimension.GetValue3((int)swInConfigurationOpts_e.swThisConfiguration, null);

                        bool check = ModelDoc2.Extension.SelectByID2(sketchName, "SKETCH", 0, 0, 0, false, 0, null, 0);
                    }

                    ModelDoc2.BlankSketch();

                }
            }

            if (flangeWidth == 0)
            {
                throw new Exception("Flange width not found.");
            }
        }
        protected bool[] SuppressFeatures(params string[] features)
        {
            bool[] results = new bool[features.Length];

            for (int i = 0; i < features.Length; i++)
            {
                string featureName = features[i];
                bool isSelected = ModelDoc2.Extension.SelectByID2(featureName, "BODYFEATURE", 0, 0, 0, false, 0, null, 0);

                if (!isSelected)
                {
                    results[i] = false;
                    continue;
                }

                var feature = ModelDoc2.ISelectionManager.GetSelectedObject6(1, -1);
                bool isCurrentlySuppressed = ((Feature)feature).IsSuppressed();

                if (!isCurrentlySuppressed)
                {
                    results[i] = ModelDoc2.EditSuppress2();
                }
                else
                {
                    results[i] = true;
                }

                ModelDoc2.ClearSelection2(true);
            }

            return results;
        }
        protected bool[] UnsuppressFeatures(params string[] features)
        {
            bool[] results = new bool[features.Length];

            for (int i = 0; i < features.Length; i++)
            {
                string featureName = features[i];
                bool isSelected = ModelDoc2.Extension.SelectByID2(featureName, "BODYFEATURE", 0, 0, 0, false, 0, null, 0);

                if (!isSelected)
                {
                    results[i] = false;
                    continue;
                }

                var feature = ModelDoc2.ISelectionManager.GetSelectedObject6(1, -1);
                bool isCurrentlySuppressed = ((Feature)feature).IsSuppressed();

                if (isCurrentlySuppressed)
                {
                    results[i] = ModelDoc2.EditUnsuppress2();
                }
                else
                {
                    results[i] = true;
                }

                ModelDoc2.ClearSelection2(true);
            }

            return results;
        }


        // Virtual methods
        protected virtual void Dimensions() { }
        protected virtual void Features() { }


        // Abstract properties
        public abstract bool Enabled { get; }
        public abstract string StaticPartNo { get; }
        public abstract Shape RawMaterialShape { get; }
        public abstract string SizeOrThickness { get; }
        public abstract List<PositionData> Position { get; }


        // Public properties
        public string PartNo
        {
            get
            {
                if (!_partNoCalculated)
                {
                    if (Enabled)
                    {


                        _partNo = GetPartNoFromAssembly(StaticPartNo, _parentAssembly);
                        if (_partNo == null)
                        {
                            _partNo = GetPartNoFromDirectory(StaticPartNo, _parentAssembly);
                            if (_partNo == null)
                            {
                                if (SizeOrThickness == "") // for purchased parts
                                {
                                    _partNo = CreateNew_SubComponentFile(StaticPartNo, _parentAssembly, GetType().Name);
                                }
                                else // for fabricated parts
                                {
                                    _partNo = CreateNew_SubComponentFile(StaticPartNo, _parentAssembly);
                                }
                                
                            }
                        }

                    }
                    _partNoCalculated = true;
                }
                return _partNo;
            }
        }
        public string FilePath
        {
            get
            {
                string _filePath = GetFilePath(PartNo, "SLDPRT");
                return _filePath;
            }
        }
        public Spec MaterialSpec { get; set; } = StaticMaterialSpec;
        public ModelDoc2 ModelDoc2 { get; set; }
        public SubAssembly ParentSubAssembly { get; set; }


        // Public Enums
        public enum Shape
        {
            Plate,
            Beam,
            Angle,
            Channel,
            Tee,
            Pipe,
            BarStock
        }
        public enum Spec
        {
            A36,
            A572_50,
            A1011_33,
            A992
        };


        // Protected properties
        protected List<PositionData> _position { get; set; }


        // Private properties
        public readonly SW_Assembly _parentAssembly;
        private string _partNo = null;
        private bool _partNoCalculated = false;
    }
}
