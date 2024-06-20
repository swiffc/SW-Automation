using Bundle.SideFrame.Derived.Children;
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
        public MountingAngle(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            bool isSmall = TubeSupportPart.Height <= 4;
            bool isExtraSmall = TubeSupportPart.Height <= 2;
            double length = isExtraSmall ? 1.5 : isSmall ? 1.75 : 3.75;
            double offset = isSmall ? length / 2 : 0.875;

            EditDimension("Leg1", "sk:L", 3);
            EditDimension("Leg2", "sk:L", isSmall ? 5 : 3);
            EditDimension("Length", "L", length);
            EditDimension("OffsetFromEdge", "sk:Leg1Hole", isSmall ? length / 2 : 0.875);
            EditDimension("Gage", "sk:Leg1Hole", 2);
            EditDimension("HoleToHole", "sk:Leg1Hole", isSmall ? 0.001 : 2);
            EditDimension("Diameter", "sk:Leg1Hole", isExtraSmall ? 0.625 : 0.6875);
            EditDimension("OffsetFromEdge", "sk:Leg2Hole", offset);
            EditDimension("Gage1", "sk:Leg2Hole", isSmall ? 2 : 1.75);
        }


        // Property overrides
        public override bool Enabled => IsSmithco;
        public override string StaticPartNo => "1561L";
        public override Shape RawMaterialShape => Shape.Angle;
        public override string SizeOrThickness => TubeSupportPart.Height > 4 ? "3x3x0.25" : "5x3x0.25";
        public override List<PositionData> Position
        {
            get
            {
                double xTranslation = Bundle.Width / 2 - SideFramePart.THK;
                double yTranslation = TubeSupportPart.Height / 2;
                double zTranslation = TubeSupportPart.THK / 2;

                return new List<PositionData>
                {
                    PositionData.Create(tX: -xTranslation, tY:- yTranslation, tZ: zTranslation),
                    PositionData.Create(tX: -xTranslation, tY: -yTranslation, rX: 180, tZ: -zTranslation),
                    PositionData.Create(tX: xTranslation, tY: -yTranslation, rY: 180, tZ: -zTranslation),
                    PositionData.Create(tX: xTranslation, tY: -yTranslation, rZ: 180, tZ: zTranslation),
                };
            }
        }


    }
}
