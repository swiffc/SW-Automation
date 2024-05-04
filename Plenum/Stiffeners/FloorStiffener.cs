using ModelTools;
using Plenum.Helpers.Static;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using aTools = ModelTools.AssemblyTools;
using cTools = ModelTools.ReleaseCOM;
using mTools = Tools.ModelTools;
using bTools = ModelTools.BendTable;
using static Plenum.Plenum;
using Plenum.Floor;
using Plenum.Floor.Derived;
using static FileTools.FileTools;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum.Stiffeners
{
    internal class FloorStiffener : Part
    {
        // Static properties
        public static bool Enabled
        {
            get
            {
                return MotorShaft_Orientation.ToLower().Contains("down") ? true : false;
            }
        }
        public override RawMaterial Shape => RawMaterial.Plate;
        public override string Size => "0.1875";
        public static double Angle { get; set; } = 45;
        internal static double Leg => 3;
        internal static double Clearance => 2.5;
        private static double HoleToEdge => 1.5;
        private static double MaxHoleSpacing => 18;
        internal static double RadialBoundary => FloorPanel.Diameter / 2 + Leg + Clearance;
        internal static double GetLength(Design callerType, out double xShift)
        {
            // Work point formed by 45 degree line from fan origin to 201 Front Plane
            mTools.AAS(Angle,
                out double x_FanOriginToStiffenerOrigin,
                out double z_FanOriginToStiffenerOrigin,
                RadialBoundary);

            // Boundary limit in Z direction
            double z_FanOriginToBounds = Plenum_Length / (Fan_Count * 2) - DividerPanel.Flange + DividerPanel.THK / 2
                - (FloorPanel.ExtensionRequired == true ? InnerFloorExtension.NominalLength : 0);

            // Boundary limit in the X direction
            double x_FanOriginToBounds = Plenum_Width / 2;
            switch (callerType)
            {
                case Design.Standard:
                    x_FanOriginToBounds += SidePanel_THK - SidePanel.Leg;
                    break;
                case Design.Johnson:
                    x_FanOriginToBounds += Beam_Depth / 2 + SidePanel_THK - SidePanel.Leg;
                    break;
                case Design.Legacy:
                    x_FanOriginToBounds += Beam_Depth / 2 - Beam_FlangeTHK * 2 - SidePanel.Leg;
                    break;
            }

            // Work point to end points found on 201 Front Plane
            double zDifference = z_FanOriginToBounds - z_FanOriginToStiffenerOrigin;
            double xDifference = x_FanOriginToBounds - x_FanOriginToStiffenerOrigin;
            mTools.AAS(90 - Angle, out _, zDifference, out double rightLength);
            mTools.AAS(Angle, out _, xDifference, out double leftLength);

            // Boundary check in the negative X direction
            double xSpanRight = zDifference;
            mTools.AAS(45, out _, out double xAdjustment, Leg);
            xSpanRight += xAdjustment;

            // Shorten length to stay in bounds
            if (xSpanRight > x_FanOriginToStiffenerOrigin)
            {
                double xFanCenterToNegativeBounds = mTools.AssemblyClearance / 2 + FloorPanel.THK + bTools.GetBendRadius(FloorPanel.THK);
                xShift = xSpanRight - x_FanOriginToStiffenerOrigin + xFanCenterToNegativeBounds;
                mTools.AAS(Angle, xShift, out _, out double rightLengthAdjustment);
                rightLength -= rightLengthAdjustment;
            }
            else
            {
                xShift = 0;
            }

            // Account for 192 splice
            double adjustment192 = 0;
            //if (FloorPanel.SpliceRequired)
            //    mTools.AAS(Angle, mTools.AssemblyClearance + FloorPanel.THK + bTools.GetBendRadius(FloorPanel.THK), out _, out adjustment192);

            // Total length
            return rightLength + leftLength
                - adjustment192
                + LengthAdjustment;
        }
        public static double LengthAdjustment { get; set; } = 0;
        public static double XShiftAdjustment { get; set; } = 0;
        public static double ZShiftAdjustment { get; set; } = 0;


        // Constructor
        public FloorStiffener(Design callerType) : base(callerType) { }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            double length = GetLength(CallerType, out _);

            mTools.EditDimension("Length", "sk:BottomFlange", length, modelDoc2);

            GetHoles(length, out double count, out double spacing);
            mTools.EditDimension("Spacing", "sk:Hole", spacing, modelDoc2);
            mTools.EditDimension("Count", "sk:Hole", count, modelDoc2);
        }

        // Private methods
        private void GetHoles(double length, out double count, out double spacing)
        {
            double effectiveLength = length - HoleToEdge * 2;
            double noSpaces = Math.Ceiling(effectiveLength / MaxHoleSpacing);
            count = noSpaces + 1;
            spacing = effectiveLength / noSpaces;
        }


        // Property overrides
        public override string StaticPartNo => "201";
        public override List<PositionData> Position
        {
            get
            {
                _position = new List<PositionData>();

                if (Enabled)
                {
                    double yTranslation = Plenum_Depth - Math.Max(EndPanel_THK, SidePanel_THK) - FloorPanel.THK;
                    double length = GetLength(CallerType, out double xShift);

                    // Locate fan center
                    var zTranslation = FanCenter.ZTranslation(CallerType);

                    // Initial location of 201 origin point in relation to fan center
                    mTools.AAS(Angle, out double xTranslation, out double zOffset, RadialBoundary);

                    // Define location of 201 end point along origin plane with respect to the above location
                    mTools.AAS(Angle, out _, out double zReference, length / 2);

                    // Refine location
                    double zBounds = Plenum_Length / (Fan_Count * 2) - DividerPanel.Flange + DividerPanel.THK / 2
                        - (FloorPanel.ExtensionRequired == true ? InnerFloorExtension.NominalLength : 0);
                    double zOffset2 = zBounds - zOffset - zReference;
                    mTools.AAS(Angle, zOffset2, out double xOffset2, out _);

                    for (int i = 0; i < Fan_Count; i++)
                    {
                        _position.Add(PositionData.Create(tX: -xTranslation - XShiftAdjustment + xOffset2 - xShift, tY: -yTranslation, tZ: zTranslation[i] + zOffset + zOffset2 + ZShiftAdjustment, rY: Angle));
                        _position.Add(PositionData.Create(tX: -xTranslation - XShiftAdjustment + xOffset2 - xShift, tY: -yTranslation, tZ: zTranslation[i] - zOffset - zOffset2 - ZShiftAdjustment, rY: 180 - Angle));
                        _position.Add(PositionData.Create(tX: xTranslation + XShiftAdjustment - xOffset2 + xShift, tY: -yTranslation, tZ: zTranslation[i] - zOffset - zOffset2 - ZShiftAdjustment, rY: Angle + 180));
                        _position.Add(PositionData.Create(tX: xTranslation + XShiftAdjustment - xOffset2 + xShift, tY: -yTranslation, tZ: zTranslation[i] + zOffset + zOffset2 + ZShiftAdjustment, rY: 360 - Angle));
                    }
                }

                return _position;
            }
        }
        protected override AssemblyDoc ParentAssembly => Plenum.AssemblyDoc;
    }
}
