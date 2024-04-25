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
using System;
using static FileTools.FileTools;
using Plenum.Floor;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum
{
    internal class DividerPanel : Part
    {
        // Static properties
        public static bool Enabled
        {
            get
            {
                return FanCount > 1 ? true : false;
            }
        }
        internal static double THK => EndPanel_THK;
        internal static double Flange => 3.5;
        internal static double LocalWidth
        {
            get
            {
                switch (PlenumDesign)
                {
                    case Design.Standard:
                        if (MidColumns)
                            return Width - Beam_WebTHK - mTools.InterferenceClearance * 2;
                        else
                            return Width - CornerAngle.Gauge * 2 + HoleToEdge * 2;

                    case Design.Johnson:
                        if (MidColumns)
                            return Width - Beam_Depth - mTools.InterferenceClearance * 2;
                        else
                            return Width + Beam_Depth - mTools.InterferenceClearance * 2;

                    case Design.Legacy:
                        if (MidColumns)
                            return Width - Beam_Depth - mTools.InterferenceClearance * 2;
                        else
                            return Width + Beam_Depth - Beam_FlangeTHK * 2 - SidePanel_THK * 2 - mTools.InterferenceClearance * 2;
                    default:
                        throw new Exception();
                }
            }
        }
        internal static double HoleToEdge => 1.5;



        // Static methods
        internal static void EditDimensions_WebHoles(ModelDoc2 modelDoc2)
        {
            double span;

            if (CallerType == Design.Johnson)
            {
                span = DividerPanel.LocalWidth / 2 - mTools.HoleToEdge_General * 4;
            }
            else
            {
                span = EndPanel.LocalWidth / 2 - Beam_FlangeWidth / 2 - mTools.HoleToEdge_General * 3 - mTools.AssemblyClearance;
            }
            mTools.HolePattern(span, out double count, out double spacing);
            mTools.EditDimension("Spacing", "sk:WebHole", spacing, modelDoc2);
            mTools.EditDimension("count", "sk:WebHole", count, modelDoc2);
        }
        internal static double BottomHoleSpan()
        {
            double span;
            if (CallerType == Design.Johnson)
            {
                span = DividerPanel.LocalWidth / 2 - mTools.HoleToEdge_General * 4;
            }
            else
            {
                span = EndPanel.LocalWidth / 2 - Beam_FlangeWidth / 2 - mTools.HoleToEdge_General * 3 - mTools.AssemblyClearance;
            }
            return span;
        }

        // Constructor
        public DividerPanel(Design callerType) : base(callerType) { }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            mTools.EditDimension("Width", "sk:Web", LocalWidth, modelDoc2);
            mTools.EditDimension("Height", "sk:Web", PlenumDepth, modelDoc2);
            mTools.EditDimension("THK", "Sheet-Metal", THK, modelDoc2);
            mTools.EditDimension("innerR", "TopFlangeR", bTable.GetBendRadius(THK), modelDoc2);
            mTools.EditDimension("innerR", "BottomFlangeR", bTable.GetBendRadius(THK), modelDoc2);

            mTools.EditDimension("Hole0", "sk:Hole", CornerAngle.HolePositions[0] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole1", "sk:Hole", CornerAngle.HolePositions[1] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole2", "sk:Hole", CornerAngle.HolePositions[2] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole3", "sk:Hole", CornerAngle.HolePositions[3] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole4", "sk:Hole", CornerAngle.HolePositions[4] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole5", "sk:Hole", CornerAngle.HolePositions[5] + CornerAngle.YTranslation, modelDoc2);

            mTools.EditDimension("PlanBraceX", "sk:Hole", GetPlanBraceHole(), modelDoc2);
            mTools.EditDimension("PlanBraceY", "sk:Hole", 4 + (!PlanBrace.Enabled ? PlenumDepth : 0), modelDoc2);

            mTools.HolePattern(BottomHoleSpan(), out double count, out double spacing);
            mTools.EditDimension("Count1", "sk:BottomHole", count, modelDoc2);
            mTools.EditDimension("Count2", "sk:BottomHole", count, modelDoc2);
            mTools.EditDimension("Spacing1", "sk:BottomHole", spacing, modelDoc2);
            mTools.EditDimension("Spacing2", "sk:BottomHole", spacing, modelDoc2);

            mTools.EditDimension("Width", "sk:ColumnCut", Beam_Depth / 2 - Beam_FlangeTHK - mTools.InterferenceClearance, modelDoc2);

            EditDimensions_WebHoles(modelDoc2);

            mTools.EditDimension("Gauge", "sk:BottomHole", THK * 2 + bTable.GetBendRadius(THK) + FloorPanel.HoleToEdge1, modelDoc2);

            mTools.EditDimension("Width", "sk:SideCut", SidePanel.Leg - SidePanel_THK, modelDoc2);        }
        protected override void FeatureSuppression(ModelDoc2 modelDoc2)
        {
            bool suppress = false;
            if (CallerType != Design.Standard)
                suppress = true;
            else
                if (Beam_FlangeWidth / 2 - 0.25 > Flange - THK / 2)
                suppress = true;

            mTools.SuppressFeatures(suppress, modelDoc2, "ColumnCut", "ColumnCutMirror");

            if (MidColumns)
                mTools.SuppressFeatures(true, modelDoc2, "SideCut");
            else
                mTools.SuppressFeatures(false, modelDoc2, "SideCut");
        }


        // Private methods
        private double GetPlanBraceHole()
        {
            //double sectionThird = Length / FanCount / 3;
            mTools.AAS(45, PlanBraceHorizontal.SectionThird - EndPanel_THK / 2, out double adjacentSide, out _);
            double value = Width - adjacentSide * 2 + (CallerType == Design.Johnson ? Beam_Depth : 0);

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
                double zTranslation = Length / 2 - EndPanel_THK / 2;


                List<PositionData> _position = new List<PositionData>();

                if (FanCount > 1)
                {
                    for (int i = 1; i < FanCount; i++)
                    {
                        zTranslation -= Length / FanCount;
                        _position.Add(PositionData.Create(tZ: zTranslation));
                    }
                }

                return _position;

            }
        }
        public override string StaticPartNo => "166";
        protected override AssemblyDoc ParentAssembly => Plenum.AssemblyDoc;
        public override RawMaterial Shape => RawMaterial.Plate;
        public override string Size => THK.ToString();


    }
}
