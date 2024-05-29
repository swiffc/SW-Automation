using FileTools.Base;
using MachineryMount.DriveWeldment;
using MachineryMount.Mechanicals;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;

namespace MachineryMount.MotorMount.Children
{
    internal class MotorMountPart : Part
    {
        // Static properties
        static public double ExtraBase => 4.625;


        // Constructor
        public MotorMountPart(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Base", "sk:Plate", Motor.Dim.O + ExtraBase);
            EditDimension("D", "sk:Plate", Motor.Dim.D);
            EditDimension("Back", "sk:Plate", Motor.Dim.C - Motor.Dim.BA - Motor.Dim.NW);
            EditDimension("Width", "Plate", DriveFrame.Width);
        }


        // Property overrides
        public override bool Enabled => ForcedDraft;
        public override string StaticPartNo => "234P";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => "0.25";
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create()
                };
            }
        }
    }
}
