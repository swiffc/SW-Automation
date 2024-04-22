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
using System.Windows.Forms;
using Plenum.Helpers.Static;
using System.Diagnostics;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum.Floor.Derived.Derived
{
    internal class OuterFloorPanel : FloorPanel
    {
        // Static properties
        public static bool Enabled
        {
            get
            {
                return CallerType == Design.Legacy ? false : true;
            }
        }
        internal override double LocalLength
        {
            get
            {
                double nominalLength = GetNominalLength(CallerType);
                double adjustedLength = CheckSectioningRequirements(nominalLength, LocalWidth);
                _length = adjustedLength;
                return (double)_length;
            }
        }
        internal static double? _length;


        // Constructor
        public OuterFloorPanel(Design callerType) : base(callerType) { }


        internal static void GetLengthAndRotation(out double zColumnnCut, out double rotate)
        {
            rotate = 0;
            if (CallerType == Design.Johnson && FanCount != 1)
            {
                var fanLocation = FanCenter.ZTranslation(CallerType);
                double firstFan = fanLocation[0];
                double lastFan = fanLocation[fanLocation.Count - 1];
                double fanSpan = firstFan + Math.Abs(lastFan);
                zColumnnCut = (Length - fanSpan) / 2;
                rotate = 90;
            }
            else
            {
                zColumnnCut = Length / FanCount / 2;
            }
        }

        // Method overrides
        protected override void EditDimensions_ColumnCut(ModelDoc2 modelDoc2)
        {
            GetLengthAndRotation(out double length, out double rotate);

            mTools.EditDimension("Width", "sk:ColumnCut", Width / 2, modelDoc2);
            mTools.EditDimension("Length", "sk:ColumnCut", length, modelDoc2);
            mTools.EditDimension("Rotate", "sk:ColumnCut", rotate, modelDoc2);

            mTools.EditDimension("Depth", "sk:ColumnCut", Beam_Depth / 2 + mTools.AssemblyClearance, modelDoc2);
            mTools.EditDimension("FlangeWidth", "sk:ColumnCut", Beam_FlangeWidth / 2 + mTools.AssemblyClearance, modelDoc2);
            mTools.EditDimension("WebTHK", "sk:ColumnCut", Beam_WebTHK / 2 + mTools.AssemblyClearance, modelDoc2);
            mTools.EditDimension("FlangeTHK", "sk:ColumnCut", Beam_FlangeTHK / 2 + mTools.AssemblyClearance, modelDoc2);
        }
        protected override void EditDimensions_FloorHoles(ModelDoc2 modelDoc2)
        {
            base.EditDimensions_FloorHoles(modelDoc2);

            double span = CallerType == Design.Johnson ? DividerPanel.LocalWidth / 2 - mTools.HoleToEdge * 3 : LocalWidth / 2 - Beam_FlangeWidth / 2 - mTools.HoleToEdge * 3 - mTools.AssemblyClearance;

            mTools.HolePattern(span, out double count2, out double spacing2);
            mTools.EditDimension("WidthSpacing", "sk:FloorHole", spacing2, modelDoc2);
            mTools.EditDimension("WidthCount", "sk:FloorHole", count2, modelDoc2);
        }
        protected override void FeatureSuppression(ModelDoc2 modelDoc2)
        {
            base.FeatureSuppression(modelDoc2);

            GetLengthAndRotation(out double zColumnCut, out _);

            double nominalLength = LocalLength + (SpliceRequired ? FloorSplice.NominalLength/2 : 0);

            if (CallerType == Design.Johnson && zColumnCut + Beam_FlangeWidth/2 < nominalLength)
                mTools.SuppressFeatures(false, modelDoc2, "ColumnCut", "JohnsonCut");
            else
                mTools.SuppressFeatures(true, modelDoc2, "ColumnCut", "JohnsonCut");
        }



        // Static methods
        internal static double GetNominalLength(Design callerType)
        {
            double calculatedLength = 0;
            switch (callerType)
            {
                case Design.Standard:
                    calculatedLength = Length / (FanCount * 2) + Beam_Depth / 2 - bTools.GetBendRadius(EndPanel_THK) - mTools.AssemblyClearance / 2;
                    break;
                case Design.Johnson:
                    if (FanCount == 1)
                    {
                        calculatedLength = Length / FanCount / 2 + Johnson.ExtraLength - bTools.GetBendRadius(EndPanel_THK) - mTools.AssemblyClearance / 2;
                        break;
                    }
                    calculatedLength = (Length / FanCount + Johnson.ExtraLength) / 2 - bTools.GetBendRadius(EndPanel_THK);
                    break;
                case Design.Legacy:
                    calculatedLength = Length / (FanCount * 2) - bTools.GetBendRadius(EndPanel_THK);
                    break;
            }
            if (calculatedLength == 0)
                Debug.WriteLine("ERROR IN OuterFloorPanel.GetNominalLength!!");

            return calculatedLength;
        }
        internal static double GetLength()
        {
            return CheckSectioningRequirements(GetNominalLength(CallerType), GetWidth(CallerType));
        }



        // Property overrides
        public override List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {
                    _position = new List<PositionData>();

                    if (CallerType != Design.Legacy)
                    {
                        var zTranslation = FanCenter.ZTranslation(CallerType);
                        double yTranslation = PlenumDepth - Math.Max(EndPanel_THK, SidePanel_THK);
                        int i = FanCount == 1 ? 0 : FanCount - 1;

                        _position.Add(PositionData.Create(tZ: zTranslation[0], tY: -yTranslation));
                        _position.Add(PositionData.Create(tZ: zTranslation[i], tY: -yTranslation, rY: 180));
                    }
                }

                return _position;
            }
        }
    }
}
