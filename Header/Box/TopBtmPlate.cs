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
    internal class TopBtmPlate : Part
    {
        // Constructor
        public TopBtmPlate(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Plate", Length, 0);
            EditDimension("Width", "sk:Plate", Width, 0);
            EditDimension("THK", "Plate", THK, 0);
        }


        // Property overrides
        public override string PartNo => Header.TopBtmPartNo;
        public override bool Enabled => true;
        public override string StaticPartNo => "TopBtmPlate";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                if (_pos == null)
                {
                    return new List<PositionData>
                    {
                        PositionData.Create(tY: -Header.BoxHeight - THK),
                        PositionData.Create(tY: THK, rZ: 180),
                    };
                }
                return _pos;
            }
        }


        // Wrapper properties
        static public double THK
        {
            get => Header.TopBtmTHK;
        }
        static public double Length
        {
            get => Header.TopBtmLength - ModelLengthReduction;
        }
        static public double Width
        {
            get => Header.TopBtmWidth > Header.BoxWidth ? Header.BoxWidth : Header.TopBtmWidth;
        }
    }
}
