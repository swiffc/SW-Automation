using FileTools.Base;
using MachineryMount.DriveWeldment;
using MachineryMount.DriveWeldment.Children;
using MachineryMount.MotorMount;
using MachineryMount.MotorMount.Children;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using  static Tools.ModelTools;

namespace MachineryMount.BeltGuard
{
    internal class BeltGuardWld : SubAssembly
    {
        // Static properties
        static public double Length => DriveFrame.Width / 2 + CrossStiffenerC.X_Translation - AssemblyClearance * 2;
        static public double Width => DriveFrame.Width - AssemblyClearance * 2;
        static public double HoleInset => 3;


        // Constructor
        public BeltGuardWld(SW_Assembly parentAssembly) : base(parentAssembly) { }


        // Property overrides
        public override bool Enabled => ForcedDraft;
        public override string StaticPartNo => "268";
        public override List<PositionData> Position
        {
            get
            {

                return new List<PositionData>
                {
                    PositionData.Create(tX: DriveFrame.Width/2 - Length/2 - AssemblyClearance, tY: 0.625)
                };
            }
        }
    }
}
