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

namespace Plenum.Floor.Derived
{
    internal abstract class InnerFloorPanel : FloorPanel
    {
        // Static properties
        public static bool Enabled
        {
            get
            {
                return FanCount > 1 || CallerType == CallerType.Legacy ? true : false;
            }
        }


        // Constructor
        public InnerFloorPanel(CallerType callerType) : base(callerType) { }


        // Method overrides
        protected override void EditDimensions_ColumnCut(ModelDoc2 modelDoc2)
        {
            mTools.EditDimension("Width", "sk:ColumnCut", Width / 2, modelDoc2);
            mTools.EditDimension("Length", "sk:ColumnCut", Length / FanCount / 2 + (CallerType == CallerType.Johnson ? Johnson.ExtraLength/2 : 0), modelDoc2);

            mTools.EditDimension("Depth", "sk:ColumnCut", Beam.Depth / 2 + mTools.AssemblyClearance, modelDoc2);
            mTools.EditDimension("FlangeWidth", "sk:ColumnCut", Beam.FlangeWidth / 2 + mTools.AssemblyClearance, modelDoc2);
            mTools.EditDimension("WebTHK", "sk:ColumnCut", Beam.WebTHK / 2 + mTools.AssemblyClearance, modelDoc2);
            mTools.EditDimension("FlangeTHK", "sk:ColumnCut", Beam.FlangeTHK / 2 + mTools.AssemblyClearance, modelDoc2);

            mTools.EditDimension("Angle", "sk:ColumnCut", CallerType == CallerType.Standard ? 90 : 180, modelDoc2);
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

            if (!MidColumns && CallerType != CallerType.Legacy)
                mTools.SuppressFeatures(true, modelDoc2, "ColumnCut");
            else if (CallerType == CallerType.Johnson && MidColumns && !ExtensionRequired)
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
                    double yTranslation = Depth - Math.Max(EndPanel.THK, SidePanel.THK);

                    for (int i = 0; i < FanCount; i++)
                    {
                        bool isNotLastForNonLegacy = CallerType != CallerType.Legacy && i != FanCount - 1;
                        bool isNotFirstForNonLegacy = CallerType != CallerType.Legacy && i != 0;

                        if (CallerType == CallerType.Legacy || isNotLastForNonLegacy)
                            _position.Add(PositionData.Create(tZ: zTranslation[i], tY: -yTranslation));

                        if (CallerType == CallerType.Legacy || isNotFirstForNonLegacy)
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
        internal static double GetNominalLength(CallerType callerType)
        {
            double calculatedLength = 0;
            switch (callerType)
            {
                case CallerType.Standard:
                    calculatedLength = Length / (FanCount * 2) - EndPanel.THK / 2 - DividerPanel.THK - bTools.GetBendRadius(EndPanel.THK) - mTools.AssemblyClearance / 2;
                    break;
                case CallerType.Johnson:
                    calculatedLength = (Length + Johnson.ExtraLength * 2) / (FanCount * 2) - DividerPanel.THK / 2 - DividerAngle.THK - bTools.GetBendRadius(DividerPanel.THK) - mTools.AssemblyClearance / 2;
                    break;
                case CallerType.Legacy:
                    calculatedLength = Length / (FanCount * 2) - bTools.GetBendRadius(EndPanel.THK) - mTools.AssemblyClearance / 2 + (FanCount > 1 ? -DividerPanel.THK / 2 - DividerFlange.THK : 0);
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
