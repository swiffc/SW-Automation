using FileTools.Base;
using HDR.Box;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using static HDR.HeaderBase;

namespace HDR.Foot
{
    internal class FootPL : Part
    {
        // Static properties
        static public double THK => 0.25;
        static public double Height => Header.FootHeight - THK;


        // Constructor
        public FootPL(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Height", "sk:Plate", Height);
        }


        // Property overrides
        public override bool Enabled =>
            IsSmithco &&
            !HeadersOutsideFrames &&
            Header.FootHeight > 0 &&
            (Header == LowestLeftHeader || Header == LowestRightHeader);
        public override string PartNo => "1502";
        public override string StaticPartNo => "FootPL";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                if (_pos == null)
                {
                    bool isWide = FootPRL.Width >= 10;

                    double xTranslation = TubeSheet.Length / 2 - FootPRL.THK;
                    double yTranslation = -(Header.BoxHeight + TopBtmPlate.THK);
                    double zTranslation = isWide ? FootPRL.Width/2 - 2 : 0;

                    _pos = new List<PositionData>
                    {
                        PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: zTranslation),
                        PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, rY: 180),
                    };

                    if (isWide)
                    {
                        _pos.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: -zTranslation));
                        _pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: -zTranslation, rY: 180));
                    }  

                }
                return _pos;
            }
        }


    }
}
