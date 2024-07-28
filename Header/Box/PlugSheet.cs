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
    internal class PlugSheet : Part
    {
        // Constructor
        public PlugSheet(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Plate", Length, 0);
            EditDimension("Width", "sk:Plate", Width, 0);
            EditDimension("TopTHK", "sk:Plate", TopBtmPlate.THK, 0);
            EditDimension("THK", "Plate", THK, 0);
        }


        // Property overrides
        public override string PartNo
        {
            get
            {
                if (Header.PlugsheetPartNo != null)
                {
                    if (Header.PlugsheetPartNo.Length != 0)
                        return Header.PlugsheetPartNo;
                }
                return "PlugSheet";
            }
        }
        public override bool Enabled => true;
        public override string StaticPartNo => "PlugSheet";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                if (_pos == null)
                {
                    double xTranslation = 0;
                    double yTranslation = 0;
                    double zTranslation = Header.BoxWidth / 2;

                    return new List<PositionData>
                    {
                        PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation),
                    };
                }
                return _pos;
            }
        }


        // Wrapper properties
        static public double THK
        {
            get => Header.PlugsheetTHK;
        }
        static public double Length
        {
            get => Header.PlugsheetLength - ModelLengthReduction;
        }
        static public double Width
        {
            get => Header.PlugsheetWidth;
        }
    }
}
