using ModelTools;
using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.InteropServices;
using static FileTools.StaticFileTools;
using static ModelTools.ReleaseCOM;

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
<<<<<<< HEAD
=======
                ParentSubAssembly = parentSubAssembly;
>>>>>>> releases/v4.0.0
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
<<<<<<< HEAD
                        Debug.WriteLine($"   Error: Dimension {equationName} could not be set. Return status: {status}");
                    }
                }
                else
                    Debug.WriteLine($"   Error: Dimension {equationName} not found.");
=======
                        Debug.WriteLine($"ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR: Dimension {equationName} could not be set. Return status: {status}");
                    }
                }
                else
                    Debug.WriteLine($"ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR: Dimension {equationName} not found.");
>>>>>>> releases/v4.0.0
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
                                _partNo = CreateNew_SubComponentFile(StaticPartNo);
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
<<<<<<< HEAD
        public ModelDoc2 ModelDoc2 {  get; set; }
=======
        public ModelDoc2 ModelDoc2 { get; set; }
        public SubAssembly ParentSubAssembly { get; set; }
>>>>>>> releases/v4.0.0


        // Public Enums
        public enum Shape
        {
            Plate,
            Beam,
            Angle,
<<<<<<< HEAD
            Channel
=======
            Channel,
            Tee
>>>>>>> releases/v4.0.0
        }
        public enum Spec
        {
            A36,
            A572_50,
            A1011_33,
            A992
        };


        // Private properties
<<<<<<< HEAD
        private readonly SW_Assembly _parentAssembly;
        private string _partNo = null;
        private bool _partNoCalculated = false;
=======
        public readonly SW_Assembly _parentAssembly;
        private string _partNo = null;
        private bool _partNoCalculated = false;


        #region Wrapper Properties

        // Structure
        protected static double Width => SharedProperties.Width;
        protected static double Length => SharedProperties.Length;
        protected static bool MidColumns => SharedProperties.MidColumns;
        protected static double ClipHeight => SharedProperties.ClipHeight;
        protected static double BraceAngle => SharedProperties.BraceAngle;
        protected static double ColumnHeight => SharedProperties.TotalColumnHeight;
        protected static string BraceType => SharedProperties.BraceType;


        // Plenum
        protected static int FanCount => SharedProperties.FanCount;
        protected static double PlenumDepth => SharedProperties.PlenumDepth;

        #endregion
>>>>>>> releases/v4.0.0
    }
}
