using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;

namespace Bundle.TubeSupports.Children
{
    internal class TubeSupport_EndPlate : Part
    {
        // Static properties
        static public double THK => 0.25;


        // Constructor
        public TubeSupport_EndPlate(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Height", "sk:Plate", TubeSupportPart.Height >= 8 ? 6.5 : 3);
            EditDimension("Offset", "sk:Plate", TubeSupportPart.Height >= 8 ? 2.25 : 1.75);
        }


        // Property overrides
        public override bool Enabled => !IsSmithco;
        public override string StaticPartNo => "1561P";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                double xTranslation = TubeSupportPart.Length / 2;
                double sealWeldOffset = 0.25;

                return new List<PositionData>
                {
                    PositionData.Create(tX: xTranslation, tY: -sealWeldOffset),
                    PositionData.Create(tX: -xTranslation, tY: -sealWeldOffset, rY: 180),
                };
            }
        }

    }
}
