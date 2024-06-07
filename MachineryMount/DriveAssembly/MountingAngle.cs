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
using SolidWorks.Interop.sldworks;

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

            HangerPRC.Holes_247(out double span, out double count, out double spacing);
            EditDimension("Offset", "sk:DriveHole", span / 2);
            EditDimension("Count", "sk:DriveHole", count);
            EditDimension("Spacing", "sk:DriveHole", spacing);

            EditDimension("Offset", "sk:PlenumHole", span / 2 + spacing / 4 );
            EditDimension("Count", "sk:PlenumHole", Math.Ceiling(count/2));
            EditDimension("Spacing", "sk:PlenumHole", spacing);
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
