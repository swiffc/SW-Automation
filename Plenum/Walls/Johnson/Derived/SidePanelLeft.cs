using ModelTools;
using SolidWorks.Interop.sldworks;
using System.Collections.Generic;
using static Plenum.Plenum;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum
{
    internal class SidePanelLeft : JohnsonSidePanel
    {
        public new static bool Enabled
        {
            get
            {
                return JohnsonSidePanel.Enabled;
            }
        }
        public SidePanelLeft(Design callerType) : base(callerType) { }
        public override string StaticPartNo { get; } = "181L";
        public override List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {
                    _position = new List<PositionData>();
                    if (FanCount > 1 && MidColumns && CallerType == Design.Johnson)
                    {
                        double xTranslation = Width / 2 + Beam_Depth / 2;
                        double zTranslation = (Length / 2) - (FanCount > 0 ? Length / (2 * FanCount) : 0);
                        _position.Add(PositionData.Create(tZ: zTranslation, tX: xTranslation));
                        _position.Add(PositionData.Create(tZ: -zTranslation, tX: -xTranslation, rY: 180));
                    }

                }
                return _position;
            }
        }

        protected override AssemblyDoc ParentAssembly => Plenum.AssemblyDoc;
    }
}
