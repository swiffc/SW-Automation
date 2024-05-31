using FileTools.Base;
using MachineryMount.MotorMount.Children;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Tools.ModelTools;

namespace MachineryMount.DriveWeldment.Children.Derived
{
    internal class DiagonalBraceLong : DiagonalBrace
    {
        // Static properties
        static public double Leg => 2.5;
        static public double THK => 0.25;
        static public double Clearance => 1;


        // Constructor
        public DiagonalBraceLong(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Distance", "sk:Path", LongDistance - Clearance * 2);
            EditDimension("DriveWidth", "sk:Path", DriveFrame.Width - WeldClearance * 2);
        }


        // Property overrides
        public override bool Enabled => LongDistance > MaxUnreinforcedDistance;
        public override string StaticPartNo => "239";
        public override Shape RawMaterialShape => Shape.Angle;
        public override string SizeOrThickness => $"{Leg}x{Leg}x{THK}";
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create
                    (
                        tX: DriveFrame.Width / 2 + CrossStiffenerC.WebTHK + LongDistance/2,
                        tY: Stringer.Depth/2 + Leg/2
                    )
                };
            }
        }
    }
}
