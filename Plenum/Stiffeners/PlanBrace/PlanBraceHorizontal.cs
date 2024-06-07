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
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;
using static FileTools.Properties.Settings;

namespace Plenum.Stiffeners
{
    internal class PlanBraceHorizontal : Part
    {
        // Static properties
        public static bool Enabled
        {
            get
            {
                return SectionThird > 66 && MotorShaftUp ? true : false;
            }
        }
        public override RawMaterial Shape => RawMaterial.Angle;
        public override string Size => "L2.5x2.5x0.1875";

        // Constructor
        public PlanBraceHorizontal(Design callerType) : base(callerType) { }


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
                    double xTranslation = 0;
                    switch (Plenum_Design)
                    {
                        case Design.Standard:
                            xTranslation = Plenum_Width / 2;
                            break;
                        case Design.Johnson:
                            xTranslation = Plenum_Width / 2 + Beam_Depth / 2;
                            break;
                        case Design.Legacy:
                            xTranslation = Plenum_Width / 2 + Beam_Depth / 2 - Beam_FlangeTHK - Default.SidePanel_THK;
                            break;
                            throw new NotImplementedException();
                    }
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
                double length = Plenum_Length + (Plenum_Design == Design.Johnson ? Default.Johnson_ExtraLength : 0) * 2;
                return length / Fan_Count / 3;
            }
        }

    }
}
