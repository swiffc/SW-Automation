using ModelTools;
using Plenum.Floor;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.FileTools;
using static Plenum.Plenum;
using aTools = ModelTools.AssemblyTools;
using cTools = ModelTools.ReleaseCOM;
using mTools = Tools.ModelTools;

namespace Plenum.JohnsonBeam.Children
{
    internal class JohnsonBeamPart : Part
    {
        // Static properties
        public static bool Enabled { get; set; } = true;
        internal static double Length => JohnsonBeamWld.Length - JohnsonBeamPlate.THK * 2;
        internal static double WebTHK => 0.25;


        // Constructor
        public JohnsonBeamPart(CallerType callerType) : base(callerType)
        {
            ChildInstances.Add(this);
        }

        public override RawMaterial Shape => RawMaterial.Beam;
        public override string Size => "W6x15";

        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            mTools.EditDimension("Length", "Beam", JohnsonBeamPart.Length, modelDoc2);

            double totalLength = Plenum.Length + Johnson.ExtraLength * 2;
            double endFanToEndPanel = totalLength / 4;
            double sectionThird = (Plenum.Length + Johnson.ExtraLength * 2) / FanCount  / 3;

            double zTotal = endFanToEndPanel - Johnson.ExtraLength;
            double z = zTotal - sectionThird / 2;

            double x = Width / 2 + Beam.Depth/2 - z + mTools.AssemblyClearance;

            mTools.EditDimension("X", "sk:Hole", x, modelDoc2);

        }
        protected override void FeatureSuppression(ModelDoc2 modelDoc2)
        {
            bool suppress = false;
            if (MotorShaft.ToLower() == "down")
                suppress = true;
            mTools.SuppressFeatures(suppress, modelDoc2, "Hole");
        }


        // Property overrides
        public override string StaticPartNo => "113P";
        public override List<PositionData> Position
        {
            get
            {
                _position = new List<PositionData>();

                if (CallerType == CallerType.Johnson)
                    _position.Add(PositionData.Create());

                return _position;
            }
        }
        protected override AssemblyDoc ParentAssembly => JohnsonBeamWld.AssemblyDoc;
    }
}
