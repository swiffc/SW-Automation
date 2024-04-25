using ModelTools;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Windows.Media.Media3D;
using System.Windows.Shapes;
using static FileTools.FileTools;
using static Plenum.Plenum;
using aTools = ModelTools.AssemblyTools;
using cTools = ModelTools.ReleaseCOM;
using mTools = Tools.ModelTools;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum
{
    internal abstract class Part : IComponentInfo
    {
        // Constructor
        protected Part(Design callerType)
        {
            CallerType = callerType;

            ModelDoc2 modelDoc2 = FTools.OpenSilent(FilePath, StaticPartNo);
            FTools.AddRawMaterialInfo(Shape, Material, Size, modelDoc2);

            EditDimensions(modelDoc2);
            FeatureSuppression(modelDoc2);

            cTools.Release(ref modelDoc2);
        }


        // Virtual methods
        protected virtual void EditDimensions(ModelDoc2 modelDoc2) { }
        protected virtual void FeatureSuppression(ModelDoc2 modelDoc2) { }


        // Abstract properties
        public abstract string StaticPartNo { get; }
        protected abstract AssemblyDoc ParentAssembly { get; }
        public abstract RawMaterial Shape {get;}
        public abstract string Size { get; }
        public abstract List<PositionData> Position { get; }


        // Non-virtual properties
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
                             ?? FTools.CreateNew_SubComponentFile(StaticPartNo);
                }
                return _partNo;
            }
        }
        protected static Design CallerType { get; set; }
        public static MaterialSpec Material { get; set; } = MaterialSpec.A36;


        // Backing fields
        private string _filePath;
        private string _partNo;
        protected List<PositionData> _position;


        // Constants
        private const string FileType = "SLDPRT";
    }
}
