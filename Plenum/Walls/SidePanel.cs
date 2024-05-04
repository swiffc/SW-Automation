using ModelTools;
using SolidWorks.Interop.sldworks;
using System.Collections.Generic;
using static Plenum.Plenum;
using aTools = ModelTools.AssemblyTools;
using cTools = ModelTools.ReleaseCOM;
using mTools = Tools.ModelTools;
using bTable = ModelTools.BendTable;
using System.Windows.Media.Imaging;
using Plenum.Floor;
using Plenum.Floor.Derived;
using System;
using static FileTools.FileTools;
using Plenum.Helpers.Static;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;
using static FileTools.Properties.Settings;

namespace Plenum
{
    internal class SidePanel : Part
    {
        // Static properties
        public static bool Enabled
        {
            get
            {
                if (CallerType == Design.Johnson && Mid_Columns && Fan_Count > 2) return true;

                return CallerType != Design.Johnson || !Mid_Columns;
            }


        }
        internal static double Leg => 3;
        internal static double Gauge => SidePanel_THK + bTable.GetBendRadius(SidePanel_THK) + FloorPanel.HoleToEdge1;
        internal static double R => bTable.GetBendRadius(SidePanel_THK);
        internal static double HoleToEdge => 1;


        // Constructor
        public SidePanel(Design callerType) : base(callerType) { }


        // Methods
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            mTools.EditDimension("Length", "sk:Web", LocalLength, modelDoc2);
            mTools.EditDimension("Height", "sk:Web", Plenum_Depth, modelDoc2);
            mTools.EditDimension("THK", "Sheet-Metal", SidePanel_THK, modelDoc2);
            mTools.EditDimension("Leg", "sk:BottomFlange", Leg, modelDoc2);
            mTools.EditDimension("innerR", "TopFlangeR", bTable.GetBendRadius(SidePanel_THK), modelDoc2);
            mTools.EditDimension("innerR", "Sheet-Metal", bTable.GetBendRadius(SidePanel_THK), modelDoc2);

            if (CallerType == Design.Legacy)
                mTools.EditDimension("DistanceToEdge", "sk:Hole", 1.25, modelDoc2);
            else
                mTools.EditDimension("DistanceToEdge", "sk:Hole", 1.5, modelDoc2);

            mTools.EditDimension("PlanBraceZ", "sk:Hole", Plenum_Length / Fan_Count / 3, modelDoc2);
            mTools.EditDimension("PlanBraceY", "sk:Hole", 4 + (!PlanBrace.Enabled ? Plenum_Depth : 0), modelDoc2);

            mTools.EditDimension("MotorBeam", "sk:Hole", MotorBeamWld.Enabled ? 7.25 : Plenum_Depth + 3, modelDoc2);

            EditDimensions_186(modelDoc2);
            EditDimensions_191(modelDoc2);
            EditDimensions_192(modelDoc2);
            EditDimensions_206(modelDoc2);

            mTools.HolePattern(LocalLength - 6, out double count, out double spacing);
            mTools.EditDimension("Spacing", "sk:BundleHole", spacing, modelDoc2);
            mTools.EditDimension("Count", "sk:BundleHole", count, modelDoc2);

            EditDimensions_NoMidColumns(modelDoc2);
        }
        internal static double CalculateXTranslation()
        {
            double xTranslation = Plenum_Width / 2;
            switch (CallerType)
            {
                case Design.Johnson:
                    xTranslation += Beam_Depth / 2;
                    break;
                case Design.Legacy:
                    xTranslation += Beam_Depth / 2 - SidePanel_THK - Beam_FlangeTHK;
                    break;
                case Design.Standard:
                    xTranslation -= SidePanel_THK;
                    break;
            }
            return xTranslation;
        }
        private static double CalculateStandardLength(double baseLength)
        {
            return baseLength - Beam_Depth - mTools.AssemblyClearance * 4;
        }
        private static double CalculateJohnsonLength(double baseLength)
        {
            double length = baseLength - mTools.AssemblyClearance * 2;

            if (Fan_Count > 1 && Mid_Columns)
            {
                length -= mTools.AssemblyClearance * 2;
            }
            else
            {
                length += Default.Johnson_ExtraLength * 2 - mTools.AssemblyClearance * 2;
            }

            return length;
        }
        private static double CalculateLegacyLength(double baseLength)
        {
            return baseLength - Beam_K1 * 2 - mTools.AssemblyClearance * 2;
        }
        private double CalculateZTranslation()
        {
            if (Mid_Columns)
            {
                return (Plenum_Length / 2) - (Fan_Count > 0 ? Plenum_Length / (2 * Fan_Count) : 0);
            }

            return 0;
        }
        private List<PositionData> InitializePositionData(double xTranslation, double zTranslation)
        {
            var positionData = new List<PositionData>();
            if (!(CallerType == Design.Johnson && Fan_Count > 1 && Mid_Columns))
            {
                positionData.Add(PositionData.Create(tZ: zTranslation, tX: xTranslation));
                positionData.Add(PositionData.Create(tZ: zTranslation, tX: -xTranslation, rY: 180));
            }
            return positionData;
        }
        private bool ShouldAddAdditionalPositions()
        {
            return Fan_Count > 1 && Mid_Columns;
        }
        private void AddAdditionalPositions(double xTranslation, ref double zTranslation)
        {
            for (int i = 1; i < Fan_Count; i++)
            {
                if (CallerType == Design.Johnson && i == Fan_Count - 1)
                {
                    continue;
                }
                zTranslation -= Plenum_Length / Fan_Count;
                _position.Add(PositionData.Create(tZ: zTranslation, tX: xTranslation));
                _position.Add(PositionData.Create(tZ: zTranslation, tX: -xTranslation, rY: 180));
            }
        }
        internal static void EditDimensions_191(ModelDoc2 modelDoc2)
        {
            double innerFloorPanelLength = InnerFloorPanel.GetLength();
            double baseOffset = innerFloorPanelLength - 3;
            double assemblyClearanceOffset = mTools.AssemblyClearance / 2;
            double floorSpliceOffset = 0;
            if (FloorPanel.SpliceRequired)
                floorSpliceOffset = FloorSplice.NominalLength / 2;
            double max = Math.Max(Beam_Depth, Beam_FlangeWidth);

            double offset = baseOffset + assemblyClearanceOffset + floorSpliceOffset;

            mTools.EditDimension("191Offset", "sk:BottomHole", offset - max / 2, modelDoc2);
            mTools.HolePattern(InnerFloorPanel.GetLength() - 3 * 2 - max / 2, out double count191, out double spacing191);
            mTools.EditDimension("191Spacing", "sk:BottomHole", spacing191, modelDoc2);
            mTools.EditDimension("191Count", "sk:BottomHole", count191, modelDoc2);

            mTools.EditDimension("191Gauge", "sk:BottomHole", Gauge, modelDoc2);
        }
        internal static void EditDimensions_192(ModelDoc2 modelDoc2)
        {
            if (FloorPanel.SpliceRequired)
            {
                mTools.EditDimension("192Gauge", "sk:BottomHole", Gauge, modelDoc2);
                mTools.EditDimension("192Offset", "sk:BottomHole", (FloorSplice.NominalLength - mTools.AssemblyClearance) / 2 - 3, modelDoc2);

                mTools.HolePattern(FloorSplice.NominalLength - mTools.AssemblyClearance - 3 * 2, out double count192, out double spacing192);
                mTools.EditDimension("192Spacing", "sk:BottomHole", spacing192, modelDoc2);
                mTools.EditDimension("192Count", "sk:BottomHole", count192, modelDoc2);
            }
            else
            {
                mTools.EditDimension("192Gauge", "sk:BottomHole", Gauge + 3, modelDoc2);
            }
        }
        internal static void EditDimensions_206(ModelDoc2 modelDoc2)
        {
            if (FloorPanel.ExtensionRequired)
            {
                mTools.EditDimension("206Gauge", "sk:BottomHole", Gauge, modelDoc2);
                mTools.EditDimension("206Offset", "sk:BottomHole", FloorSplice.NominalLength / 2 + InnerFloorPanel.GetLength() + 3 + mTools.AssemblyClearance, modelDoc2);

                double sideFrameLength = CallerType == Design.Johnson ? JohnsonSidePanel.LocalLength - Beam_FlangeWidth / 2 - 1.5 : SidePanel.LocalLength;
                mTools.HolePattern(sideFrameLength / 2 - (FloorSplice.NominalLength / 2 + InnerFloorPanel.GetLength()) - 3 * 2, out double count206, out double spacing206);
                mTools.EditDimension("206Spacing", "sk:BottomHole", spacing206, modelDoc2);
                mTools.EditDimension("206Count", "sk:BottomHole", count206, modelDoc2);
            }
            else
            {
                mTools.EditDimension("206Gauge", "sk:BottomHole", Gauge + 3, modelDoc2);
            }
        }
        private void EditDimensions_186(ModelDoc2 modelDoc2)
        {
            mTools.EditDimension("HoleA", "sk:Hole", CornerAngle.HolePositions[6] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("HoleB", "sk:Hole", CornerAngle.HolePositions[7] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("HoleC", "sk:Hole", CornerAngle.HolePositions[8] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("HoleD", "sk:Hole", CornerAngle.HolePositions[9] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("HoleE", "sk:Hole", CornerAngle.HolePositions[10] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("HoleF", "sk:Hole", Plenum_Depth + 1, modelDoc2);
        }
        private void EditDimensions_NoMidColumns(ModelDoc2 modelDoc2)
        {
            double holeToEdge = CallerType == Design.Legacy ? 1.25 : 1.5;

            double angleSpacing;
            if (Mid_Columns)
                angleSpacing = LocalLength - holeToEdge * 2;
            else
                angleSpacing = (LocalLength - holeToEdge * 2) / Fan_Count + DividerPanel.THK / 2 + CornerAngle.Gauge;
            mTools.EditDimension("CornerAngleSpacing", "sk:Hole", angleSpacing, modelDoc2);


            var fanSpacingList = FanCenter.ZTranslation(CallerType);
            double fanSpacing = fanSpacingList[0] - fanSpacingList[fanSpacingList.Count - 1];

            double fanLocation = (LocalLength / 2) - (Mid_Columns ? 0 : fanSpacing / Fan_Count);
            mTools.EditDimension("FanLocation", "sk:Hole", fanLocation, modelDoc2);
            mTools.EditDimension("FanLocation", "sk:BottomHole", fanLocation, modelDoc2);


            if (fanSpacing == 0)
                fanSpacing = LocalLength;
            mTools.EditDimension("FanSpacing", "sk:Hole", fanSpacing, modelDoc2);
            mTools.EditDimension("FanSpacing", "sk:BottomHole", fanSpacing, modelDoc2);


            double count;
            if (Fan_Count < 2)
                count = 2;
            else
                count = Fan_Count;
            mTools.EditDimension("CornerAngleCount", "sk:Hole", count, modelDoc2);
            mTools.EditDimension("FanCount", "sk:Hole", count, modelDoc2);
            mTools.EditDimension("FanCount", "sk:BottomHole", count, modelDoc2);



        }


        // Public properties
        public override List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {
                    double xTranslation = CalculateXTranslation();
                    double zTranslation = CalculateZTranslation();

                    _position = InitializePositionData(xTranslation, zTranslation);

                    if (ShouldAddAdditionalPositions())
                    {
                        AddAdditionalPositions(xTranslation, ref zTranslation);
                    }
                }

                return _position;
            }
        }
        public override RawMaterial Shape => RawMaterial.Plate;
        public override string Size => SidePanel_THK.ToString();


        // Internal properties
        internal static double LocalLength
        {
            get
            {
                double denominator = Mid_Columns ? Fan_Count : 1;
                double baseLength = Plenum_Length / denominator;

                switch (CallerType)
                {
                    case Design.Standard:
                        return CalculateStandardLength(baseLength);

                    case Design.Johnson:
                        return CalculateJohnsonLength(baseLength);

                    case Design.Legacy:
                        return CalculateLegacyLength(baseLength);

                    default:
                        throw new NotImplementedException();
                }
            }
        }



        // Constants
        public override string StaticPartNo => "181";
        protected override AssemblyDoc ParentAssembly => Plenum.AssemblyDoc;
    }
}
