using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using static Tools.ModelTools;

namespace MachineryMount.DriveWeldment.Children
{
    internal class HangerStiffener : Part
    {
        // Constructor
        public HangerStiffener(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Plate", HangerPRC.Width - HangerPRC.THK * 2 - WeldClearance * 2);
        }


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => "248";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => HangerPRC.THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                double xTranslation = Stringer.Length / 2 - HangerPRC.THK;
                double yTranslation = HangerPRC.THK * 1.5;

                return new List<PositionData>
                {
                    PositionData.Create(tX: xTranslation , tY: yTranslation),
                    PositionData.Create(tX: -xTranslation, tY: yTranslation, rY: 180)
                };
            }
        }
    }
}
