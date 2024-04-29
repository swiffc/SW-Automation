using ModelTools;
using Plenum.Helpers.Static;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Plenum.Plenum;
using static System.Windows.Forms.VisualStyles.VisualStyleElement.Window;
using aTools = ModelTools.AssemblyTools;
using cTools = ModelTools.ReleaseCOM;
using mTools = Tools.ModelTools;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum
{
    internal class PlanBraceMiddle : PlanBrace
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
        public PlanBraceMiddle(Design callerType) : base(callerType) { }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            double length = GetNominalLength();
            AdjustLengthZ(-EndPanel_THK / 2, ref length);

            mTools.EditDimension("Length", "L", length, modelDoc2);
        }


        // Property overrides
        public override string StaticPartNo => "179";
        public override List<PositionData> Position
        {
            get
            {
                _position = new List<PositionData>();

                if (Enabled)
                {
                    double adjust = EndPanel_THK / 4;

                    GetPositionAtNominalLength(out double xTranslation, out double yTranslation, out double angle);
                    AdjustPositionZ(adjust, ref xTranslation);

                    var zTranslations = FanCenter.ZTranslation(CallerType);

                    double sectionThird = (Plenum_Length / Fan_Count + (CallerType == Design.Johnson ? Johnson.ExtraLength : 0)) / 3;
                    double positionAdjust = adjust;

                    for (int i = 0; i < Fan_Count; i++)
                    {
                        if (i != 0 && i != Fan_Count - 1 && CallerType == Design.Johnson)
                            continue;

                        if (i != Fan_Count - 1 // This block will run for every iteration except the last one,
                            || CallerType == Design.Legacy) // unless CallerType is Legacy, in which case it runs for all iterations including the last one.
                        {
                            _position.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslations[i] - sectionThird + positionAdjust, rY: -angle));
                            _position.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: zTranslations[i] - sectionThird + positionAdjust, rY: -angle - 90));
                        }

                        if (i != 0 // This block will run for every iteration except the first one,
                            || CallerType == Design.Legacy) // unless CallerType is Legacy, in which case it runs for all iterations including the first one.)
                        {
                            _position.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: zTranslations[i] + sectionThird - positionAdjust, rY: -angle - 180));
                            _position.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslations[i] + sectionThird - positionAdjust, rY: -angle - 270));
                        }
                    }
                }

                return _position;
            }
        }
    }
}
