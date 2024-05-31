using FileTools.Base;
using MachineryMount.DriveWeldment.Children;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using static Tools.ModelTools;
using static ModelTools.BendTable;
using MachineryMount.DriveWeldment;

namespace MachineryMount.DriveAssembly
{
    internal class MountingAngle : Part
    {
        // Constructor
        public MountingAngle(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Path", HangerPRC.Width - HangerPRC.THK * 2 - GetBendRadius(HangerPRC.THK) * 2);
        }


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => "247";
        public override Shape RawMaterialShape => Shape.Angle;
        public override string SizeOrThickness => "3x3x0.25";
        public override List<PositionData> Position
        {
            get
            {
                double xTranslation = Plenum_Width / 2 - HangerPRC.THK;
                double yTranslation = DriveFrame.Height;

                return new List<PositionData>
                {
                    PositionData.Create(tX: xTranslation, tY: yTranslation),
                    PositionData.Create(tX: -xTranslation, tY: yTranslation, rY: 180)
                };
            }
        }
    }
}
