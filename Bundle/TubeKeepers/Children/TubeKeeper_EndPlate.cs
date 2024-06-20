using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;

namespace Bundle.TubeKeepers.Children
{
    internal class TubeKeeper_EndPlate : Part
    {
        // Static properties
        static public double THK => 0.25;


        // Constructor
        public TubeKeeper_EndPlate(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("OffsetFromBottom", "sk:Plate", TubeKeeperWeldment.IsLarge ? 1.5 : 1);
        }


        // Property overrides
        public override bool Enabled => !IsSmithco;
        public override string StaticPartNo => "1505";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                double xTranslation = TubeKeeperPipe.Length / 2;
                double yTranslation = TubeKeeperPipe.OD / 2;

                return new List<PositionData>
                {
                    PositionData.Create(tX: xTranslation, tY: yTranslation),
                    PositionData.Create(tX: -xTranslation, tY: yTranslation, rY: 180),
                };
            }
        }
    }
}
