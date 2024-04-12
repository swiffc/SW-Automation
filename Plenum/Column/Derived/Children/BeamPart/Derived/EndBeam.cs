using ModelTools;
using Plenum;
using SolidWorks.Interop.sldworks;
using System.Collections.Generic;
using static FileTools.FileTools;
using static Plenum.Plenum;
using mTools = Tools.ModelTools;

namespace Plenum
{
    internal class EndBeam : Beam
    {
        public static bool Enabled { get; set; } = true;
        internal static AssemblyDoc CallerDoc { get; set; }
        public EndBeam(CallerType callerType) : base(callerType)
        {
            ChildInstances.Add(this);
        }
        public override string StaticPartNo => "117P";

        protected override AssemblyDoc ParentAssembly => CallerDoc;
        protected override void FeatureSuppression(ModelDoc2 modelDoc2)
        {

            switch (CallerType)
            {
                case CallerType.Standard:
                    mTools.SuppressFeatures(false, modelDoc2, "XYmirror");
                    mTools.SuppressFeatures(true, modelDoc2, "YZmirror");
                    mTools.SuppressFeatures(true, modelDoc2, "JohnsonHole");
                    mTools.SuppressFeatures(true, modelDoc2, "JohnsonHoles");
                    break;
                case CallerType.Johnson:
                    mTools.SuppressFeatures(false, modelDoc2, "YZmirror");
                    mTools.SuppressFeatures(true, modelDoc2, "XYmirror");
                    mTools.SuppressFeatures(false, modelDoc2, "JohnsonHole");
                    mTools.SuppressFeatures(false, modelDoc2, "JohnsonHoles");
                    break;
                case CallerType.Legacy:
                    mTools.SuppressFeatures(true, modelDoc2, "YZmirror");
                    mTools.SuppressFeatures(false, modelDoc2, "XYmirror");
                    mTools.SuppressFeatures(true, modelDoc2, "JohnsonHole");
                    mTools.SuppressFeatures(true, modelDoc2, "JohnsonHoles");
                    break;
            }
            
        }

    }
}