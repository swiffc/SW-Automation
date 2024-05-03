using ModelTools;
using System.Collections.Generic;
using static FileTools.StaticFileTools;
using static ModelTools.ReleaseCOM;
using static Tools.ModelTools;

namespace FileTools.Base
{
    public abstract class SubAssembly : SW_Assembly, IComponentInfo2
    {
        // Constructor
        protected SubAssembly(SW_Assembly parentAssembly)
        {
            if (Enabled)
            {
                _parentAssembly = parentAssembly;
                AssemblyDoc = OpenAssembly(FilePath, StaticPartNo, false);
                var subComponents = InstantiateSubComponents(GetType(), this);
                PlaceSubComponents(subComponents, this);
                if (parentAssembly is MainAssembly)
                    foreach (var subComponent in subComponents)
                        parentAssembly.GrandChildren.Add(subComponent);
                Release(AssemblyDoc);
            }
        }


        // Abstract properties
        public abstract bool Enabled { get; }
        public abstract string StaticPartNo { get; }
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
                                _partNo = CreateNew_ComponentFile(StaticPartNo);
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
                string _filePath = GetFilePath(PartNo, "SLDASM");
                return _filePath;
            }
        }


        // Private properties
        private readonly SW_Assembly _parentAssembly;
        private string _partNo = null;
        private bool _partNoCalculated = false;


        #region Wrapper Properties

<<<<<<< HEAD
        // Footprint
        public double Width => SharedProperties.Width;
        public double Length => SharedProperties.Length;
        public bool MidColumns => SharedProperties.MidColumns;


        // Plenum
        public int FanCount => SharedProperties.FanCount;
=======
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
>>>>>>> releases/v4.0.0

        #endregion
    }
}
