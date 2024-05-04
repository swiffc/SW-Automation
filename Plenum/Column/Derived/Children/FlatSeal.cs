using ModelTools;
using SolidWorks.Interop.sldworks;
using System.Collections.Generic;
using static FileTools.FileTools;
using static Plenum.Plenum;
using aTools = ModelTools.AssemblyTools;
using cTools = ModelTools.ReleaseCOM;
using mTools = Tools.ModelTools;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum
{
    internal class FlatSeal : Part
    {
        // Static properties
        public static bool Enabled { get; set; } = false;
        internal static AssemblyDoc CallerDoc { get; set; }


        // Constructor
        public FlatSeal(Design callerType) : base(callerType)
        {
            ChildInstances.Add(this);
        }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {

            mTools.EditDimension("Depth", "sk:Beam", Beam_Depth, modelDoc2);
            mTools.EditDimension("WebTHK", "sk:Beam", Beam_WebTHK, modelDoc2);
            mTools.EditDimension("FlangeWidth", "sk:Beam", Beam_FlangeWidth, modelDoc2);
            mTools.EditDimension("FlangeTHK", "sk:Beam", Beam_FlangeTHK, modelDoc2);
            mTools.EditDimension("K", "sk:Beam", Beam_K, modelDoc2);
            mTools.EditDimension("K1", "sk:Beam", Beam_K1, modelDoc2);
        }


        // Public properties
        public override List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {

                    _position =  new List<PositionData>();
                    if (CallerType == Design.Legacy)
                    {
                        _position.Add(PositionData.Create());
                    }
                    

                }
                return _position;
            }
        }
        public override string StaticPartNo { get; } = "114";
        protected override AssemblyDoc ParentAssembly => CallerDoc;
        public override RawMaterial Shape => RawMaterial.Plate;
        public override string Size => "0.1344";
    }
}
