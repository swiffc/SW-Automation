using ModelTools;
using Plenum.Helpers.Static;
using Plenum.JohnsonBeam.Children;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Plenum.Plenum;
using aTools = ModelTools.AssemblyTools;
using cTools = ModelTools.ReleaseCOM;
using mTools = Tools.ModelTools;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;
using static FileTools.Properties.Settings;

namespace Plenum
{
    internal class PlanBraceEnd : PlanBrace
    {
        public new static bool Enabled
        {
            get
            {
                bool enabled = PlanBrace.Enabled;
                return enabled;
            }
        }

        // Constructor
        public PlanBraceEnd(Design callerType) : base(callerType) { }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            double adjust = CallerType == Design.Johnson ? -(Default.Johnson_ExtraLength + JohnsonBeamPart.WebTHK / 2) : Beam_Depth / 2;

            double length = GetNominalLength();
            AdjustLengthZ(adjust, ref length);

            mTools.EditDimension("Length", "L", length, modelDoc2);
        }


        // Property overrides
        public override string StaticPartNo => "178";
        public override List<PositionData> Position
        {
            get
            {
                _position = new List<PositionData>();

                if (CallerType != Design.Legacy)
                {
                    double adjust = CallerType == Design.Johnson ? -(Default.Johnson_ExtraLength + JohnsonBeamPart.WebTHK / 2) / 2 : Beam_Depth / 4;

                    GetPositionAtNominalLength(out double xTranslation, out double yTranslation, out double angle, true);
                    xTranslation -= adjust;

                    var zTranslations = FanCenter.ZTranslation(CallerType);

                    double sectionThird = (Plenum_Length / Fan_Count + (CallerType == Design.Johnson ? Default.Johnson_ExtraLength : 0)) / 3;
                    double positionAdjust = -adjust;

                    _position.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: zTranslations[0] + sectionThird - positionAdjust, rY: -angle - 180));
                    _position.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslations[0] + sectionThird - positionAdjust, rY: -angle - 270));

                    _position.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: zTranslations[zTranslations.Count - 1] - sectionThird + positionAdjust, rY: -angle - 90));
                    _position.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslations[zTranslations.Count - 1] - sectionThird + positionAdjust, rY: -angle));
                }

                return _position;
            }
        }
    }
}
