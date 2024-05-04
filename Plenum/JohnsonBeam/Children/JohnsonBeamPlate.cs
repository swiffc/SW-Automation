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
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum.JohnsonBeam.Children
{
    internal class JohnsonBeamPlate : Part
    {
        // Static properties
        public static bool Enabled { get; set; } = true;
        internal static double THK => 0.5;


        // Constructor
        public JohnsonBeamPlate(Design callerType) : base(callerType)
        {
            ChildInstances.Add(this);
        }

        public override RawMaterial Shape => RawMaterial.Plate;
        public override string Size => "0.5000";
        // Property overrides
        public override string StaticPartNo => "112";
        public override List<PositionData> Position
        {
            get
            {
                _position = new List<PositionData>();

                if (CallerType == Design.Johnson)
                {
                    double xTranslation = JohnsonBeamPart.Length / 2;

                    _position.Add(PositionData.Create(tX: xTranslation));
                    _position.Add(PositionData.Create(tX: -xTranslation, rY: 180));
                }

                return _position;
            }
        }
        protected override AssemblyDoc ParentAssembly => JohnsonBeamWld.AssemblyDoc;
    }
}
