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
        // Static properties
        static public bool IsSmall => TubeSupportPart.Height <= 4;
        static public bool IsExtraSmall => TubeSupportPart.Height <= 2;
        static public double HoleToHole => IsSmall ? 0.001 : 2;
        static public double Gage => IsSmall ? 2 : 1.75;


        // Constructor
        public MountingAngle(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            double length = IsExtraSmall ? 1.5 : IsSmall ? 1.75 : 3.75;
            double offset = IsSmall ? length / 2 : 0.875;

            EditDimension("Leg1", "sk:L", 3);
            EditDimension("Leg2", "sk:L", IsSmall ? 5 : 3);
            EditDimension("Length", "L", length);
            EditDimension("OffsetFromEdge", "sk:Leg1Hole", IsSmall ? length / 2 : 0.875);
            EditDimension("Gage", "sk:Leg1Hole", 2);
            EditDimension("HoleToHole", "sk:Leg1Hole", HoleToHole);
            EditDimension("Diameter", "sk:Leg1Hole", IsExtraSmall ? 0.625 : 0.6875);
            EditDimension("OffsetFromEdge", "sk:Leg2Hole", offset);
            EditDimension("Gage1", "sk:Leg2Hole", Gage);
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
