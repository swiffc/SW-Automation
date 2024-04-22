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

        // Structure
        protected static double Width => CommonData.CommonData.Width;
        protected static double Length => CommonData.CommonData.Length;
        protected static bool MidColumns => CommonData.CommonData.MidColumns;
        protected static double ClipHeight => CommonData.CommonData.ClipHeight;
        protected static double BraceAngle => CommonData.CommonData.BraceAngle;
        protected static double ColumnHeight => CommonData.CommonData.TotalColumnHeight;
        protected static string BraceType => CommonData.CommonData.BraceType;


        // Plenum
        protected static int FanCount => CommonData.CommonData.FanCount;
        protected static double PlenumDepth => CommonData.CommonData.PlenumDepth;

        #endregion
    }
}
