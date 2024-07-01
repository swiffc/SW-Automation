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
    internal class TubeSheet : Part
    {
        // Constructor
        public TubeSheet(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Plate", Length);
            EditDimension("Width", "sk:Plate", Width);
            EditDimension("Thickness", "sk:Plate", THK);
            //EditDimension("X", "sk:Plate", );
        }


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => "TubeSheet";
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
                    double zTranslation = 0;

                    return new List<PositionData>
                    {
                        PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation),
                    };
                }
                return _pos;
            }
        }


        // Wrapper properties
        // Static properties
        static public double THK
        {
            get => Header.TubesheetTHK;
            set => Header.TubesheetTHK = value;
        }
        static public double Length
        {
            get => Header.TubesheetLength;
            set => Header.TubesheetLength = value;
        }
        static public double Width
        {
            get => Header.TubesheetWidth;
            set => Header.TubesheetWidth = value;
        }
    }
}
