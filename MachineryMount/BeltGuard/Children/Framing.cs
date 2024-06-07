using FileTools.Base;
using MachineryMount.DriveWeldment;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;


namespace MachineryMount.BeltGuard.Children
{
    internal class Framing : Part
    {
        // Static properties
        static public double THK => 0.1344;


        // Constructor
        public Framing(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Width", "sk:Base", BeltGuardWld.Width);
            EditDimension("Length", "sk:Base", BeltGuardWld.Length);
        }


        // Property overrides
        public override bool Enabled => ForcedDraft;
        public override string StaticPartNo => "268P";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(),
                    PositionData.Create(rY: 180),
                };
            }
        }
    }
}
