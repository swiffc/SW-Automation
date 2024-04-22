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
    // Constructor
    internal class MotorBeamPlate : Part
    {
        // Static properties
        public static bool Enabled { get; set; } = MotorBeamWld.Enabled;
        internal static double THK { get; set; } = 0.25;


        // Constructor
        public MotorBeamPlate(Design callerType) : base(callerType)
        {
            ChildInstances.Add(this);
        }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            mTools.EditDimension("THK", "Plate", THK, modelDoc2);
        }


        // Property overrides
        public override List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {
                    double xTranslation = MotorBeamWld.LocalLength / 2;
                    return new List<PositionData>
                    {
                        PositionData.Create(tX: -xTranslation),
                        PositionData.Create(tX: xTranslation, rY: 180)
                    };

                }
                return _position;
            }
        }
        public override string StaticPartNo => "267";
        protected override AssemblyDoc ParentAssembly => MotorBeamWld.AssemblyDoc;
        public override RawMaterial Shape => RawMaterial.Plate;
        public override string Size => THK.ToString();
    }
}
