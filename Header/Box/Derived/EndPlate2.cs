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
        public override bool Enabled => Header.EndPlateBustedSpan2 != 0 && IsBusted;
        public override string PartNo
        {
            get
            {
                if (Header.EndPlatePartNo2 != null)
                {
                    if (Header.EndPlatePartNo2.Length != 0)
                        return Header.EndPlatePartNo2;
                }
                return "Endplate2";
            }
        }
        protected override double LocalLength => Length2;
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
                        yTranslation = - Length1 - Length2 / 2 - (Header.BoxHeight - Length1 - Length2);
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


        // Wrapper properties
        public double Length2 => Header.EndPlateBustedSpan2;

    }
}
