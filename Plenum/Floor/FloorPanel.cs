using ModelTools;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Plenum.Plenum;
using aTools = ModelTools.AssemblyTools;
using cTools = ModelTools.ReleaseCOM;
using mTools = Tools.ModelTools;
using bTools = ModelTools.BendTable;
using System.Diagnostics;
using Plenum.Stiffeners;
using System.Windows.Markup;
using Plenum.Floor.Derived;
using static FileTools.FileTools;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum.Floor
{
    internal abstract class FloorPanel : Part
    {

        // Static properties
        internal static double FlangeHeight => 3;
        internal static double FlangeToOpening => 6;
        internal static double THK => 0.1344;
        internal static double Diameter
        {
            get
            {
                (double minTipClearance, double maxTipClearance) = mTools.GetTipClearance(FanDiameter_Inches);
                double averageTipClearance = (minTipClearance + maxTipClearance) / 2;
                return FanDiameter_Inches + averageTipClearance * 2 + mTools.InterferenceClearance;
            }
        }
        internal static double HoleToEdge1 => 1;
        internal static double HoleToEdge3 => 3;



        // Static methods
        internal static double GetWidth(Design callerType)
        {
            var value = Plenum_Width / 2 - bTools.GetBendRadius(SidePanel_THK) - mTools.AssemblyClearance / 2;

            if (PlenumDesign == Design.Standard)
                value -= SidePanel_THK;

            else if (PlenumDesign == Design.Johnson)
                value += Beam_Depth / 2;

            else if (PlenumDesign == Design.Legacy)
                value += Beam_Depth / 2 - Beam_FlangeTHK - SidePanel_THK;

            return value;
        }


        // Constructor
        public FloorPanel(Design callerType) : base(callerType) { }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            mTools.EditDimension("Diameter", "sk:Plate", Diameter, modelDoc2);
            mTools.EditDimension("Length", "sk:Plate", LocalLength, modelDoc2);
            mTools.EditDimension("Width", "sk:Plate", LocalWidth, modelDoc2);

            mTools.EditDimension("SideGauge", "sk:FloorHole", HoleToEdge1, modelDoc2);

            // Private methods
            EditDimensions_SpliceRequired(modelDoc2);
            EditDimensions_MotorShaft(modelDoc2);
            EditDimensions_FloorStiffener(modelDoc2);
            EditDimensions_ExtensionRequired(modelDoc2);

            // Abstract/virtual methods
            EditDimensions_ColumnCut(modelDoc2);
            EditDimensions_FloorHoles(modelDoc2);
        }
        protected override void FeatureSuppression(ModelDoc2 modelDoc2)
        {
            if (ExtensionRequired)
            {
                mTools.SuppressFeatures_Legacy(true, modelDoc2, "MidFlange");
                mTools.SuppressFeatures_Legacy(true, modelDoc2, "MidFlangeHole");
                mTools.SuppressFeatures_Legacy(true, modelDoc2, "MidFlangeHoles");
                if (CallerType != Design.Johnson)
                    mTools.SuppressFeatures_Legacy(true, modelDoc2, "ColumnCut");
                mTools.SuppressFeatures_Legacy(false, modelDoc2, "ExtensionFlange");
                mTools.SuppressFeatures_Legacy(false, modelDoc2, "ExtensionFlangeHole");
                mTools.SuppressFeatures_Legacy(false, modelDoc2, "ExtensionFlangeHoles");
            }
            else
            {
                mTools.SuppressFeatures_Legacy(false, modelDoc2, "MidFlange");
                mTools.SuppressFeatures_Legacy(false, modelDoc2, "MidFlangeHole");
                mTools.SuppressFeatures_Legacy(false, modelDoc2, "MidFlangeHoles");
                if (CallerType != Design.Johnson)
                    mTools.SuppressFeatures_Legacy(false, modelDoc2, "ColumnCut");
                bool[] check1 = mTools.SuppressFeatures_Legacy(true, modelDoc2, "ExtensionFlange");
                bool[] check2 = mTools.SuppressFeatures_Legacy(true, modelDoc2, "ExtensionFlangeHole");
                bool[] check3 = mTools.SuppressFeatures_Legacy(true, modelDoc2, "ExtensionFlangeHoles");
                Console.WriteLine();
            }


            if (MotorShaftDown)
            {
                mTools.SuppressFeatures_Legacy(false, modelDoc2, "ShaftDownRadialHole");
                mTools.SuppressFeatures_Legacy(false, modelDoc2, "ShaftDownRadialHoles");
                mTools.SuppressFeatures_Legacy(true, modelDoc2, "ShaftUpRadialHole");
                mTools.SuppressFeatures_Legacy(true, modelDoc2, "ShaftUpRadialHoles");
            }
            else
            {
                mTools.SuppressFeatures_Legacy(true, modelDoc2, "ShaftDownRadialHole");
                mTools.SuppressFeatures_Legacy(true, modelDoc2, "ShaftDownRadialHoles");
                mTools.SuppressFeatures_Legacy(false, modelDoc2, "ShaftUpRadialHole");
                mTools.SuppressFeatures_Legacy(false, modelDoc2, "ShaftUpRadialHoles");
            }
        }


        // Internal methods
        internal static void ClearBackingFields()
        {
            _spliceRequired = null;
        }


        // Private methods
        private void EditDimensions_FloorStiffener(ModelDoc2 modelDoc2)
        {
            if (FloorStiffener.Enabled)
            {
                // Origin of 201 FloorStiffener
                mTools.AAS(FloorStiffener.Angle, out double x1, out double z1, FloorStiffener.RadialBoundary);

                // Move to hole gauge
                mTools.AAS(FloorStiffener.Angle, out double x2, out double z2, 1.375);

                // Move to origin hole
                double patternLength = FloorStiffener.GetLength(CallerType, out _) - 1.5 * 2;
                mTools.AAS(FloorStiffener.Angle, out double x3, out double z3, patternLength / 2);

                // Initial location of  201 origin point in relation to fan center
                mTools.AAS(FloorStiffener.Angle, out _, out double zOffset, FloorStiffener.RadialBoundary);

                // Define location of 201 end point along origin plane with respect to the above location
                double length = FloorStiffener.GetLength(CallerType, out double xShift);
                mTools.AAS(FloorStiffener.Angle, out _, out double zReference, length / 2);

                // Refine location
                double zBounds = Plenum_Length / (Fan_Count * 2) - DividerPanel.Flange + DividerPanel.THK / 2
                    - (FloorPanel.ExtensionRequired == true ? InnerFloorExtension.NominalLength : 0);
                double zOffset2 = zBounds - zOffset - zReference;
                mTools.AAS(FloorStiffener.Angle, zOffset2, out double xOffset2, out _);

                mTools.EditDimension("Z", "sk:FloorHole", z1 - z2 + z3 + zOffset2 + FloorStiffener.ZShiftAdjustment, modelDoc2);
                mTools.EditDimension("X", "sk:FloorHole", x1 - x2 - x3 - xOffset2 + xShift + FloorStiffener.XShiftAdjustment, modelDoc2);

                mTools.HolePattern(patternLength, out double count, out double spacing, 18);
                mTools.EditDimension("201Count", "sk:FloorHole", count, modelDoc2);
                mTools.EditDimension("201Spacing", "sk:FloorHole", spacing, modelDoc2);
            }
            else
            {
                mTools.EditDimension("Z", "sk:FloorHole", 1, modelDoc2);
                mTools.EditDimension("X", "sk:FloorHole", 1, modelDoc2);

                mTools.EditDimension("201Count", "sk:FloorHole", 2, modelDoc2);
                mTools.EditDimension("201Spacing", "sk:FloorHole", 1, modelDoc2);
            }
        }
        private void EditDimensions_SpliceRequired(ModelDoc2 modelDoc2)
        {
            double flange192Span = LocalWidth - Diameter / 2 - 1.5 * 2 + 0.0625
                + (SpliceRequired == true ? 1.7283 : 0); // Measurement from Solidworks for 30" long 192splice
            mTools.HolePattern(flange192Span, out double count192, out double spacing192);
            mTools.EditDimension("Count", "sk:192FlangeHole", count192, modelDoc2);
            mTools.EditDimension("Spacing", "sk:192FlangeHole", spacing192, modelDoc2);

            if (SpliceRequired == true)
            {
                mTools.EditDimension("192Gap", "sk:Plate", FloorSplice.NominalLength / 2 + mTools.AssemblyClearance / 2, modelDoc2);
            }
            else
            {
                mTools.EditDimension("192Gap", "sk:Plate", mTools.AssemblyClearance / 2, modelDoc2);
            }
        }
        private void EditDimensions_MotorShaft(ModelDoc2 modelDoc2)
        {
            if (MotorShaftDown)
            {
                mTools.EditDimension("Count", "sk:ShaftDownRadialHole", FanRing.RadialCount.ShaftDown, modelDoc2);
                mTools.EditDimension("BoltCircleR", "sk:ShaftDownRadialHole", FanRing.Radius + 1.125, modelDoc2);
                mTools.EditDimension("Angle", "sk:ShaftDownRadialHole", 360 / FanRing.RadialCount.ShaftDown / 2, modelDoc2);
            }
            else
            {
                mTools.EditDimension("Count", "ShaftUpRadialHoles", FanRing.RadialCount.ShaftUp, modelDoc2);
                mTools.EditDimension("BoltCircleR", "sk:RadialCurve", FanDiameter_Inches / 2 + 0.375 + 1.125, modelDoc2);
            }
        }
        private void EditDimensions_ExtensionRequired(ModelDoc2 modelDoc2)
        {
            if (ExtensionRequired)
            {
                mTools.EditDimension("WidthGauge", "sk:FloorHole", 1.875 + LocalLength, modelDoc2);

                mTools.HolePattern(LocalWidth - 1.5 * 2, out double count, out double spacing);
                mTools.EditDimension("Count", "sk:ExtensionFlangeHole", count, modelDoc2);
                mTools.EditDimension("Spacing", "sk:ExtensionFlangeHole", spacing, modelDoc2);
            }
            else
            {
                // Mid flange holes
                double midFlangeSpan = LocalLength - Diameter / 2 - 1.5 * 2 + 0.0625
                    + (SpliceRequired == true ? FloorSplice.NominalLength / 2 : 0);
                mTools.HolePattern(midFlangeSpan, out double midCount, out double midSpacing);
                mTools.EditDimension("Count", "sk:MidFlangeHole", midCount, modelDoc2);
                mTools.EditDimension("Spacing", "sk:MidFlangeHole", midSpacing, modelDoc2);

                mTools.EditDimension("WidthGauge", "sk:FloorHole", HoleToEdge1, modelDoc2);
            }

        }


        // Abstract/virtual methods
        protected abstract void EditDimensions_ColumnCut(ModelDoc2 modelDoc2);
        protected virtual void EditDimensions_FloorHoles(ModelDoc2 modelDoc2)
        {
            double max = Math.Max(Beam_Depth, Beam_FlangeWidth);
            mTools.HolePattern(InnerFloorPanel.GetLength() - HoleToEdge3 * 2 - max / 2, out double count1, out double spacing1);
            mTools.EditDimension("LengthSpacing", "sk:FloorHole", spacing1, modelDoc2);
            mTools.EditDimension("LengthCount", "sk:FloorHole", count1, modelDoc2);
        }


        // Internal methods
        internal static double CheckSectioningRequirements(double panelLength, double panelWidth)
        {
            double widthLimit = mTools.MaxSheetWidth - 2;
            SpliceRequired = false;
            ExtensionRequired = false;

            double check = _fanDiameterFeet;

            if (_fanDiameterFeet > 6 && Plenum_Width / 2 - FanRing.Radius < 12)
            {
                SpliceRequired = true;
                panelLength -= FloorSplice.NominalLength / 2;
            }

            if (panelWidth > widthLimit)
            {
                if (panelLength > widthLimit && !SpliceRequired)
                {
                    SpliceRequired = true;
                    panelLength -= FloorSplice.NominalLength / 2;
                }

                if (panelLength > widthLimit)
                {
                    ExtensionRequired = true;
                    panelLength = FanDiameter_Inches / 2 + FlangeToOpening + FlangeHeight - FloorSplice.NominalLength / 2;
                }

                // If all options are exhausted, force length into nesting limitations.
                // This will result in part of the file being cut off as a visual cue 
                // for the user to make manual modifications.
                if (panelLength > widthLimit)
                {
                    panelLength = widthLimit;
                }
            }

            return panelLength;
        }


        // Property overrides
        public override string StaticPartNo { get; }
        protected override AssemblyDoc ParentAssembly => Plenum.AssemblyDoc;
        public override RawMaterial Shape => RawMaterial.Plate;
        public override string Size => THK.ToString();


        // Internal properties
        private static bool? _spliceRequired;
        public static bool SpliceRequired
        {
            get
            {
                if (_spliceRequired.HasValue)
                {
                    return _spliceRequired.Value;
                }
                double length = InnerFloorPanel.GetNominalLength(CallerType);
                double maxWidth = mTools.MaxSheetWidth - 1;
                return length > maxWidth;
            }
            set
            {
                _spliceRequired = value;
            }
        }
        private static bool? _extensionRequired;
        internal static bool ExtensionRequired
        {
            get
            {
                if (_extensionRequired.HasValue)
                {
                    return _extensionRequired.Value;
                }

                double length = InnerFloorPanel.GetNominalLength(CallerType) - FloorSplice.NominalLength / 2;
                double maxWidth = mTools.MaxSheetWidth - 1;
                bool returnValue = length > maxWidth;
                return returnValue;
            }
            set
            {
                _extensionRequired = value;
            }
        }

        internal double LocalWidth
        {
            get
            {
                return GetWidth(CallerType);
            }
        }


        // Abstract properties
        internal abstract double LocalLength { get; }
    }
}
