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
    // Constructor
    internal class MotorBeamPart : Part
    {
        // Static properties
        public static bool Enabled { get; set; } = true;


        // Constructor
        public MotorBeamPart(CallerType callerType) : base(callerType)
        {
            ChildInstances.Add(this);
        }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            mTools.EditDimension("Length", "Beam", MotorBeamWld.LocalLength - MotorBeamPlate.THK * 2, modelDoc2);
        }


        // Property overrides
        public override List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {
                    return new List<PositionData>
                    {
                        PositionData.Create()
                    };

                }
                return _position;
            }
        }
        public override string StaticPartNo =>"266P";
        protected override AssemblyDoc ParentAssembly => MotorBeamWld.AssemblyDoc;
        public override RawMaterial Shape => RawMaterial.Beam;
        public override string Size => "W6x15";
    }
}
