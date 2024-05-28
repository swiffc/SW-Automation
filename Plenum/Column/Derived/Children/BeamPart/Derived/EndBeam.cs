using ModelTools;
using Plenum;
using SolidWorks.Interop.sldworks;
using System.Collections.Generic;
using static FileTools.FileTools;
using static Plenum.Plenum;
using mTools = Tools.ModelTools;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum
{
    internal class EndBeam : Beam
    {
        public static bool Enabled { get; set; } = true;
        internal static AssemblyDoc CallerDoc { get; set; }
        public EndBeam(Design callerType) : base(callerType)
        {
            ChildInstances.Add(this);
        }
        public override string StaticPartNo => "117P";

        protected override AssemblyDoc ParentAssembly => CallerDoc;
        protected override void FeatureSuppression(ModelDoc2 modelDoc2)
        {

            switch (CallerType)
            {
                case Design.Standard:
                    mTools.SuppressFeatures_Legacy(false, modelDoc2, "XYmirror");
                    mTools.SuppressFeatures_Legacy(true, modelDoc2, "YZmirror");
                    mTools.SuppressFeatures_Legacy(true, modelDoc2, "JohnsonHole");
                    mTools.SuppressFeatures_Legacy(true, modelDoc2, "JohnsonHoles");
                    break;
                case Design.Johnson:
                    mTools.SuppressFeatures_Legacy(false, modelDoc2, "YZmirror");
                    mTools.SuppressFeatures_Legacy(true, modelDoc2, "XYmirror");
                    mTools.SuppressFeatures_Legacy(false, modelDoc2, "JohnsonHole");
                    mTools.SuppressFeatures_Legacy(false, modelDoc2, "JohnsonHoles");
                    break;
                case Design.Legacy:
                    mTools.SuppressFeatures_Legacy(true, modelDoc2, "YZmirror");
                    mTools.SuppressFeatures_Legacy(false, modelDoc2, "XYmirror");
                    mTools.SuppressFeatures_Legacy(true, modelDoc2, "JohnsonHole");
                    mTools.SuppressFeatures_Legacy(true, modelDoc2, "JohnsonHoles");
                    break;
            }
            
        }

    }
}