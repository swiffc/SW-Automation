using ModelTools;
using SolidWorks.Interop.sldworks;
using System.Collections.Generic;
using static FileTools.FileTools;
using static Plenum.Plenum;
using aTools = ModelTools.AssemblyTools;
using cTools = ModelTools.ReleaseCOM;
using mTools = Tools.ModelTools;

namespace Plenum
{
    internal class JohnsonTopPlate : Part
    {
        // Static properties
        public static bool Enabled
        {
            get
            {
                return CallerType == CallerType.Johnson ? true : false;
            }
        }
        internal static AssemblyDoc CallerDoc { get; set; }


        // Constructor
        public JohnsonTopPlate(CallerType callerType) : base(callerType)
        {
            ChildInstances.Add(this);
        }


        // Private methods
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            mTools.EditDimension("Width", "sk:Plate", Beam.FlangeWidth, modelDoc2);
            mTools.EditDimension("Depth", "sk:Plate", Beam.Depth, modelDoc2);
        }


        // Property overrides
        public override List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {
                    _position = new List<PositionData>();

                    if (CallerType == CallerType.Johnson)
                    {

                        _position.Add(PositionData.Create());

                    }
                }
                return _position;
            }
        }
        public override string StaticPartNo => "115";
        protected override AssemblyDoc ParentAssembly => CallerDoc;
        public override RawMaterial Shape => RawMaterial.Plate;

        public override string Size => "0.5000";
    }
}
