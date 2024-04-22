using ModelTools;
using SolidWorks.Interop.sldworks;
using System.Collections.Generic;
using static Plenum.Plenum;
using aTools = ModelTools.AssemblyTools;
using cTools = ModelTools.ReleaseCOM;
using mTools = Tools.ModelTools;
using bTable = ModelTools.BendTable;
using System.Windows.Media.Imaging;
using Plenum.Stiffeners;
using static FileTools.FileTools;
using Plenum.Floor;
using System;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum
{
    internal class EndPanel : Part
    {
        // Static properties
        public static bool Enabled { get; set; } = true;
        internal static double BottomLeg
        {
            get
            {
                double defaultValue = 3;
                double minimumValue = Gauge + 0.9375;
                if (minimumValue > defaultValue)
                    return Math.Ceiling(minimumValue / 0.125) * 0.125;
                else
                    return defaultValue;
            }
        }
        internal static double Gauge
        {
            get
            {
                double value = EndPanel_THK + bTable.GetBendRadius(EndPanel_THK) + FloorPanel.HoleToEdge1;
                if (CallerType == Design.Legacy && FanCount > 1)
                    value += EndPanel_THK / 2 + EndPanel_THK;
                return value;
            }
        }
        internal static double LocalWidth
        {
            get
            {
                switch (CallerType)
                {
                    case Design.Standard:
                        return Width - mTools.AssemblyClearance * 2;
                    case Design.Johnson:
                        return Width + Beam_Depth - mTools.AssemblyClearance * 2;
                    case Design.Legacy:
                        return Width - Beam_Depth - mTools.AssemblyClearance * 2;
                    default:
                        throw new KeyNotFoundException();
                }
            }
        }
        internal static double HoleToEdge => 1;


        // Constructor
        public EndPanel(Design callerType) : base(callerType) { }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            mTools.EditDimension("Width", "sk:Web", LocalWidth, modelDoc2);
            mTools.EditDimension("Height", "sk:Web", PlenumDepth, modelDoc2);
            mTools.EditDimension("THK", "Sheet-Metal", EndPanel_THK, modelDoc2);
            mTools.EditDimension("innerR", "TopFlangeR", bTable.GetBendRadius(EndPanel_THK), modelDoc2);
            mTools.EditDimension("innerR", "BottomFlangeR", bTable.GetBendRadius(EndPanel_THK), modelDoc2);
            mTools.EditDimension("Leg", "sk:BottomFlange", BottomLeg, modelDoc2);

            mTools.EditDimension("Hole0", "sk:Hole", CornerAngle.HolePositions[0] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole1", "sk:Hole", CornerAngle.HolePositions[1] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole2", "sk:Hole", CornerAngle.HolePositions[2] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole3", "sk:Hole", CornerAngle.HolePositions[3] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole4", "sk:Hole", CornerAngle.HolePositions[4] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole5", "sk:Hole", CornerAngle.HolePositions[5] + CornerAngle.YTranslation, modelDoc2);

            mTools.EditDimension("PlanBraceX", "sk:Hole", GetPlanBraceHole(), modelDoc2);
            mTools.EditDimension("PlanBraceY", "sk:Hole", 4 + (!PlanBrace.Enabled || CallerType == Design.Johnson ? PlenumDepth : 0), modelDoc2);

            double span = CallerType == Design.Johnson ? DividerPanel.LocalWidth / 2 - mTools.HoleToEdge * 3 : LocalWidth / 2 - Beam_FlangeWidth / 2 - mTools.HoleToEdge * 3 - mTools.AssemblyClearance;

            mTools.HolePattern(span, out double count, out double spacing);
            mTools.EditDimension("Count1", "sk:BottomHole", count, modelDoc2);
            mTools.EditDimension("Count2", "sk:BottomHole", count, modelDoc2);
            mTools.EditDimension("Spacing1", "sk:BottomHole", spacing, modelDoc2);
            mTools.EditDimension("Spacing2", "sk:BottomHole", spacing, modelDoc2);

            mTools.EditDimension("Width", "sk:ColumnCut", Beam_FlangeWidth / 2, modelDoc2);

            mTools.EditDimension("Gauge", "sk:BottomHole", Gauge, modelDoc2);

        }
        protected override void FeatureSuppression(ModelDoc2 modelDoc2)
        {
            switch (CallerType)
            {
                case Design.Standard:
                    mTools.SuppressFeatures(false, modelDoc2, "ColumnCut", "1", "2", "3", "4");
                    break;
                case Design.Johnson:
                    mTools.SuppressFeatures(false, modelDoc2, "ColumnCut", "1", "2", "3", "4");
                    break;
                case Design.Legacy:
                    mTools.SuppressFeatures(true, modelDoc2, "ColumnCut", "1", "2", "3", "4");
                    break;
            }
        }


        // Static methods
        public static double CalculateZTranslation()
        {
            double lengthModifier = 0;
            switch (CallerType)
            {
                case Design.Standard:
                    lengthModifier = Beam_Depth / 2;
                    break;
                case Design.Johnson:
                    lengthModifier = Johnson.ExtraLength;
                    break;
            }
            double length = Length / 2 + lengthModifier;

            return length;
        }


        // Private methods
        private double GetPlanBraceHole()
        {
            double sectionThird = Length / FanCount / 3;
            mTools.AAS(45, sectionThird + Beam_Depth / 2, out double adjacentSide, out _);
            double value = Width - adjacentSide * 2;
            double filteredValue = value;
            if (value < 6 && PlanBraceHorizontal.Enabled)
            {
                filteredValue = 6;
            }
            else if (value < 3 && !PlanBraceHorizontal.Enabled)
            {
                filteredValue = 3;
            }
            return filteredValue;
        }


        // Property overrides
        public override List<PositionData> Position
        {
            get
            {
                if (_position != null)
                {
                    return _position;
                }

                double length = CalculateZTranslation();

                _position = new List<PositionData>
                {
                    PositionData.Create(tZ: length),
                    PositionData.Create(tZ: -length, rY: 180)
                };

                return _position;
            }
        }
        public override string StaticPartNo => "156";
        protected override AssemblyDoc ParentAssembly => Plenum.AssemblyDoc;
        public override RawMaterial Shape => RawMaterial.Plate;
        public override string Size => EndPanel_THK.ToString();
    }
}
