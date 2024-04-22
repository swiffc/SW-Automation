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
using Plenum.Helpers.Static;
using Plenum.Floor.Derived.Derived;
using static FileTools.FileTools;
using Plenum.Walls;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum.Floor.Derived
{
    internal abstract class InnerFloorPanel : FloorPanel
    {
        // Static properties
        public static bool Enabled
        {
            get
            {
                return FanCount > 1 || CallerType == Design.Legacy ? true : false;
            }
        }


        // Constructor
        public InnerFloorPanel(Design callerType) : base(callerType) { }


        // Method overrides
        protected override void EditDimensions_ColumnCut(ModelDoc2 modelDoc2)
        {
            mTools.EditDimension("Width", "sk:ColumnCut", Width / 2, modelDoc2);
            mTools.EditDimension("Length", "sk:ColumnCut", Length / FanCount / 2 + (CallerType == Design.Johnson ? Johnson.ExtraLength/2 : 0), modelDoc2);

            mTools.EditDimension("Depth", "sk:ColumnCut", Beam_Depth / 2 + mTools.AssemblyClearance, modelDoc2);
            mTools.EditDimension("FlangeWidth", "sk:ColumnCut", Beam_FlangeWidth / 2 + mTools.AssemblyClearance, modelDoc2);
            mTools.EditDimension("WebTHK", "sk:ColumnCut", Beam_WebTHK / 2 + mTools.AssemblyClearance, modelDoc2);
            mTools.EditDimension("FlangeTHK", "sk:ColumnCut", Beam_FlangeTHK / 2 + mTools.AssemblyClearance, modelDoc2);

            mTools.EditDimension("Angle", "sk:ColumnCut", CallerType == Design.Standard ? 90 : 180, modelDoc2);
        }
        protected override void EditDimensions_FloorHoles(ModelDoc2 modelDoc2)
        {
            base.EditDimensions_FloorHoles(modelDoc2);
            mTools.HolePattern(DividerPanel.BottomHoleSpan(), out double count2, out double spacing2);
            mTools.EditDimension("WidthSpacing", "sk:FloorHole", spacing2, modelDoc2);
            mTools.EditDimension("WidthCount", "sk:FloorHole", count2, modelDoc2);
        }
        protected override void FeatureSuppression(ModelDoc2 modelDoc2)
        {
            base.FeatureSuppression(modelDoc2);

            if (!MidColumns && CallerType != Design.Legacy)
                mTools.SuppressFeatures(true, modelDoc2, "ColumnCut");
            else if (CallerType == Design.Johnson && MidColumns && !ExtensionRequired)
                mTools.SuppressFeatures(false, modelDoc2, "ColumnCut");
        }

        public override RawMaterial Shape => base.Shape;


        // Property overrides
        public override List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {
                    _position = new List<PositionData>();

                    var zTranslation = FanCenter.ZTranslation(CallerType);
                    double yTranslation = PlenumDepth - Math.Max(EndPanel_THK, SidePanel_THK);

                    for (int i = 0; i < FanCount; i++)
                    {
                        bool isNotLastForNonLegacy = CallerType != Design.Legacy && i != FanCount - 1;
                        bool isNotFirstForNonLegacy = CallerType != Design.Legacy && i != 0;

                        if (CallerType == Design.Legacy || isNotLastForNonLegacy)
                            _position.Add(PositionData.Create(tZ: zTranslation[i], tY: -yTranslation));

                        if (CallerType == Design.Legacy || isNotFirstForNonLegacy)
                            _position.Add(PositionData.Create(tZ: zTranslation[i], tY: -yTranslation, rY: 180));
                    }
                }

                return _position;
            }
        }
        internal override double LocalLength
        {
            get
            {
                _length = CheckSectioningRequirements(GetNominalLength(CallerType), LocalWidth);
                return (double)_length;
            }
        }
        private static double? _length;


        // Static methods                
        internal static double GetNominalLength(Design callerType)
        {
            double calculatedLength = 0;
            switch (callerType)
            {
                case Design.Standard:
                    calculatedLength = Length / (FanCount * 2) - EndPanel_THK / 2 - DividerPanel.THK - bTools.GetBendRadius(EndPanel_THK) - mTools.AssemblyClearance / 2;
                    break;
                case Design.Johnson:
                    calculatedLength = (Length + Johnson.ExtraLength * 2) / (FanCount * 2) - DividerPanel.THK / 2 - DividerAngle.THK - bTools.GetBendRadius(DividerPanel.THK) - mTools.AssemblyClearance / 2;
                    break;
                case Design.Legacy:
                    calculatedLength = Length / (FanCount * 2) - bTools.GetBendRadius(EndPanel_THK) - mTools.AssemblyClearance / 2 + (FanCount > 1 ? -DividerPanel.THK / 2 - DividerFlange.THK : 0);
                    break;
            }
            return calculatedLength;
        }



        internal static double GetLength()
        {
            double nominalLength = GetNominalLength(CallerType);
            double adjustedLength = CheckSectioningRequirements(nominalLength, GetWidth(CallerType));
            return adjustedLength;
        }
    }
}
