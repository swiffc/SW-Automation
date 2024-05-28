using ModelTools;
using SolidWorks.Interop.sldworks;
using System.Collections.Generic;
using static Plenum.Plenum;
using mTools = Tools.ModelTools;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum
{
    internal class MidBeam : Beam
    {
        public static bool Enabled { get; set; } = true;
        internal static AssemblyDoc CallerDoc { get; set; }
        public MidBeam(Design callerType) : base(callerType)
        {
            ChildInstances.Add(this);
        }
        public override string StaticPartNo => "116P";
        public EndBeam Beam { get; set; }

        protected override AssemblyDoc ParentAssembly => CallerDoc;
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            base.EditDimensions(modelDoc2);
            mTools.EditDimension("Gage", "sk:WebHole", DividerPanel.THK / 2 + DividerAngle.ShortGauge, modelDoc2);
        }
        protected override void FeatureSuppression(ModelDoc2 modelDoc2)
        {
            if (CallerType == Design.Legacy || CallerType == Design.Johnson)
            {
                bool[] check1 = mTools.SuppressFeatures_Legacy(true, modelDoc2, "WebHole", "WebHoles");
                bool[] check2 = mTools.SuppressFeatures_Legacy(false, modelDoc2, "YZmirror");
            }
            else
            {
                bool[] check1 = mTools.SuppressFeatures_Legacy(false, modelDoc2, "WebHole", "WebHoles");
                bool[] check2 = mTools.SuppressFeatures_Legacy(true, modelDoc2, "YZmirror");
            }
        }
        protected override void AdjustWebHoles(ModelDoc2 modelDoc2)
        {
            mTools.EditDimension("Hole6", "sk:WebHole", CornerAngle.HolePositions[6] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole7", "sk:WebHole", CornerAngle.HolePositions[7] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole8", "sk:WebHole", CornerAngle.HolePositions[8] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole9", "sk:WebHole", CornerAngle.HolePositions[9] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole10", "sk:WebHole", CornerAngle.HolePositions[10] + CornerAngle.YTranslation, modelDoc2);
        }
    }
}
