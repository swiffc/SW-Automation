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
    internal class FootPRL : Part
    {
        // Static properties
        static public double Width => Header.BoxWidth + TubeSheet.THK + PlugSheet.THK;
        static public double THK => 0.25;
        static public double Height => Header.FootHeight;


        // Constructor
        public FootPRL(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Height", "sk:Plate", Height);
            EditDimension("Width", "Plate", Width);
        }


        // Property overrides
        public override bool Enabled => 
            IsSmithco && 
            !HeadersOutsideFrames && 
            Header.FootHeight > 0 &&
            (Header == LowestLeftHeader || Header == LowestRightHeader);
        public override string PartNo => "1501";
        public override string StaticPartNo => "FootPRL";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                if (_pos == null)
                {
                    double xTranslation = TubeSheet.Length / 2;
                    double yTranslation = -(Header.BoxHeight + TopBtmPlate.THK);
                    double zTranslation = 0;

                    _pos = new List<PositionData>
                    {
                        PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: zTranslation),
                        PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, rY: 180),
                    };
                }
                return _pos;
            }
        }


    }
}
