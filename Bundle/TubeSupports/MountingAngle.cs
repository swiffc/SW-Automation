using Bundle.TubeSupports.Children;
using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;

namespace Bundle.TubeSupports
{
    internal class MountingAngle : Part
    {
        // Constructor
        public MountingAngle(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Property overrides
        public override bool Enabled => IsSmithco;
        public override string StaticPartNo => "1561L";
        public override Shape RawMaterialShape => Shape.Angle;
        public override string SizeOrThickness => TubeSupportPart.ShapeHeight > 4 ? "3x3x0.25" : "2x2x1/4";
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create(),
                };
            }
        }

        
    }
}
