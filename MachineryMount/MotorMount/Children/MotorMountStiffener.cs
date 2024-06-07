using FileTools.Base;
using MachineryMount.DriveWeldment.Children;
using MachineryMount.DriveAssembly;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using static Tools.ModelTools;

namespace MachineryMount.MotorMount.Children
{
    internal class MotorMountStiffener : Part
    {
        // Constructor
        public MotorMountStiffener(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Back", "sk:Plate", MotorMountPart.Back - MotorMountPart.THK - AssemblyClearance);
        }


        // Property overrides
        public override bool Enabled => ForcedDraft;
        public override string StaticPartNo => "235";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => MotorMountPart.THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(tX: -Motor.Dim.D - MotorMountPart.THK, tY: AssemblyClearance)
                };
            }
        }
    }
}
