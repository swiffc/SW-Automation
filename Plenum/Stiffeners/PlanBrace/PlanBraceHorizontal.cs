using ModelTools;
using Plenum.Helpers.Static;
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

namespace Plenum.Stiffeners
{
    internal class PlanBraceHorizontal : Part
    {
        // Static properties
        public static bool Enabled
        {
            get
            {
                return SectionThird > 66 && MotorShaft.ToLower() == "up" ? true : false;
            }
        }
        public override RawMaterial Shape => RawMaterial.Angle;
        public override string Size => "L2.5x2.5x0.1875";

        // Constructor
        public PlanBraceHorizontal(CallerType callerType) : base(callerType) { }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            mTools.EditDimension("Length", "L", SectionThird - 3, modelDoc2);
        }


        // Property overrides
        public override string StaticPartNo => "180";
        public override List<PositionData> Position
        {
            get
            {
                _position = new List<PositionData>();

                if (Enabled)
                {
                    double xTranslation = Width / 2 + (CallerType == CallerType.Johnson ? Beam.Depth / 2 : 0);
                    double yTranslation = -4;
                    var zTranslation = FanCenter.ZTranslation(CallerType);

                    for (int i = 0; i < zTranslation.Count; i++)
                    {
                        _position.Add(PositionData.Create(tZ: zTranslation[i], tX: xTranslation, tY: yTranslation));
                        _position.Add(PositionData.Create(tZ: zTranslation[i], tX: -xTranslation, rY: 180, tY: yTranslation));
                    }
                }

                return _position;
            }
        }
        protected override AssemblyDoc ParentAssembly => Plenum.AssemblyDoc;


        // Private properties
        internal static double SectionThird
        {
            get
            {
                double length = Length + (StaticCaller == CallerType.Johnson ? Johnson.ExtraLength : 0) * 2;
                return length / FanCount / 3;
            }
        }

    }
}
