using ModelTools;
using SolidWorks.Interop.sldworks;
using System.Collections.Generic;
using static Plenum.Plenum;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum
{
    internal class SidePanelRight : JohnsonSidePanel
    {
        public new static bool Enabled
        {
            get
            {
                return JohnsonSidePanel.Enabled;
            }
        }
        public SidePanelRight(Design callerType) : base(callerType) { }
        public override string StaticPartNo { get; } = "181R";
        public override List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {
                    _position = new List<PositionData>();
                    if (Fan_Count > 1 && Mid_Columns && CallerType == Design.Johnson)
                    {
                        double xTranslation = Plenum_Width / 2 + Beam_Depth / 2;
                        double zTranslation = (Plenum_Length / 2) - (Fan_Count > 0 ? Plenum_Length / (2 * Fan_Count) : 0);
                        _position.Add(PositionData.Create(tZ: -zTranslation, tX: xTranslation));
                        _position.Add(PositionData.Create(tZ: zTranslation, tX: -xTranslation, rY: 180));
                    }

                }
                return _position;
            }
        }

        protected override AssemblyDoc ParentAssembly => Plenum.AssemblyDoc;
    }
}
