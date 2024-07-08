using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static HDR.HeaderBase;

namespace HDR.Box.Derived
{
    internal class EndPlate2 : EndPlate
    {
        // Constructor
        public EndPlate2(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Property overrides
        public override bool Enabled => BustSpans.Count >= 2;
        public override double Length => BustSpans[1];
        public override string PartNo => "AE";
        public override string StaticPartNo => "EndPlate";
        public override List<PositionData> Position
        {
            get
            {
                if (_pos == null)
                {
                    double xTranslation = TubeSheet.Length / 2;
                    double yTranslation;
                    if (Header.IsBusted)
                    {
                        yTranslation = - BustSpans[0] - BustSpans[1] / 2 - (Header.BoxHeight - BustSpans[0] - BustSpans[1]);
                    }
                    else
                    {
                        yTranslation = -Header.BoxHeight / 2;
                    }
                    double zTranslation = 0;

                    return new List<PositionData>
                    {
                        PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation),
                        PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: zTranslation, rY: 180),
                    };
                }
                return _pos;
            }
        }


    }
}
