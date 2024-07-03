using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static HDR.HeaderBase;

namespace HDR.Box
{
    internal class EndPlate : Part
    {
        // Constructor
        public EndPlate(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Plate", Length, 0);
            EditDimension("Width", "sk:Plate", Width, 0);
            EditDimension("THK", "Plate", THK, 0);
        }


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => "EndPlate";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                if (_pos == null)
                {
                    double xTranslation = TubeSheet.Length / 2;
                    double yTranslation = -Header.BoxHeight / 2;

                    return new List<PositionData>
                    {
                        PositionData.Create(tX: xTranslation, tY: yTranslation),
                        PositionData.Create(tX: -xTranslation, tY: yTranslation, rY: 180),
                    };
                }
                return _pos;
            }
        }


        // Wrapper properties
        static public double THK
        {
            get => Header.EndPlateTHK;
        }
        static public double Length
        {
            get => Header.EndPlateLength;
        }
        static public double Width
        {
            get => Header.EndPlateWidth > Header.BoxWidth ? Header.BoxWidth : Header.EndPlateWidth;
        }
    }
}
