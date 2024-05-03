using ModelTools;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Plenum.Plenum;
using mTools = Tools.ModelTools;
using cTools = ModelTools.ReleaseCOM;
using bTable = ModelTools.BendTable;
using static FileTools.FileTools;
using Plenum.Floor;

namespace Plenum.Walls
{
    internal class DividerFlange : Part
    {
        // Static properties
        public static bool Enabled
        {
            get
            {
                return FanCount > 1 ? true : false;
            }
        }
        internal static double THK => DividerPanel.THK;
        internal static double Flange => 3;


        // Constructor
        public DividerFlange(CallerType callerType) : base(callerType) { }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            mTools.EditDimension("THK", "Sheet-Metal", THK, modelDoc2);
            mTools.EditDimension("R", "TopFlangeR", bTable.GetBendRadius(THK), modelDoc2);

            mTools.EditDimension("Length", "sk:BottomFlange", Length, modelDoc2);
            mTools.EditDimension("Leg", "sk:TopFlange", 3, modelDoc2);

            mTools.HolePattern(DividerPanel.BottomHoleSpan(), out double count, out double spacing);
            mTools.EditDimension("Count1", "sk:BottomHole", count, modelDoc2);
            mTools.EditDimension("Count2", "sk:BottomHole", count, modelDoc2);
            mTools.EditDimension("Spacing1", "sk:BottomHole", spacing, modelDoc2);
            mTools.EditDimension("Spacing2", "sk:BottomHole", spacing, modelDoc2);

            double maxValue = DividerPanel.THK / 2 + Flange;
            double valueLimit = Beam.Depth / 2 - Beam.FlangeTHK - mTools.InterferenceClearance;
            if ((CallerType != CallerType.Standard && MidColumns))
                mTools.EditDimension("Width", "sk:ColumnSeal", maxValue, modelDoc2);
            else if (maxValue > valueLimit)
                mTools.EditDimension("Width", "sk:ColumnSeal", valueLimit, modelDoc2);
            else
                mTools.EditDimension("Width", "sk:ColumnSeal", maxValue, modelDoc2);

            mTools.EditDimension("Extension", "ColumnSeal", (CallerType == CallerType.Standard ? Beam.FlangeWidth / 2 : Beam.Depth / 2) - mTools.AssemblyClearance, modelDoc2);

            DividerPanel.EditDimensions_WebHoles(modelDoc2);

            mTools.EditDimension("Gauge", "sk:BottomHole", THK + bTable.GetBendRadius(THK) + FloorPanel.HoleToEdge1, modelDoc2);
        }
        protected override void FeatureSuppression(ModelDoc2 modelDoc2)
        {
            bool suppress = true;
            if (CallerType == CallerType.Standard && MidColumns)
                suppress = false;
            else if (CallerType == CallerType.Legacy && MidColumns)
                suppress = false;
            else if (CallerType == CallerType.Johnson && MidColumns)
                suppress = false;

            mTools.SuppressFeatures(suppress, modelDoc2, "ColumnSeal", "1", "2", "3");
        }


        // Private properties
        private double Length
        {
            get
            {
                double length;
                if (CallerType == CallerType.Standard)
                    if (MidColumns)
                        length = Width - Beam.FlangeWidth - mTools.InterferenceClearance * 2;
                    else
                        length = Width - CornerAngle.Leg * 2 - mTools.AssemblyClearance * 2;

                else if (CallerType == CallerType.Johnson && !MidColumns)
                    length = Width + Beam.Depth - CornerAngle.Leg * 2 - mTools.InterferenceClearance * 2;
                else if (CallerType == CallerType.Johnson && MidColumns)
                    length = Width - Beam.Depth - DividerAngle.LongLeg * 2 - mTools.AssemblyClearance * 3;

                else if (CallerType == CallerType.Legacy && !MidColumns)
                    length = Width + Beam.Depth - Beam.FlangeTHK * 2 - SidePanel.THK * 2 - CornerAngle.Leg * 2 - mTools.AssemblyClearance * 2;
                else if (CallerType == CallerType.Legacy && MidColumns)
                    length = Width - Beam.Depth - DividerAngle.LongLeg * 2 - mTools.AssemblyClearance * 3;

                else
                    length = Width - Beam.Depth - mTools.AssemblyClearance * 4;
                return length;
            }
        }


        // Property overrides
        public override string StaticPartNo { get; } = "176P";
        protected override AssemblyDoc ParentAssembly => Plenum.AssemblyDoc;
        public override List<PositionData> Position
        {
            get
            {

                double zTranslation = Length / 2 - SidePanel.THK / 2;


                List<PositionData> _position = new List<PositionData>();

                if (FanCount > 1)
                {
                    for (int i = 1; i < FanCount; i++)
                    {
                        zTranslation -= Length / FanCount;
                        _position.Add(PositionData.Create(tZ: zTranslation, tY: -Depth));
                    }
                }

                return _position;

            }
        }
        public override RawMaterial Shape => RawMaterial.Plate;
        public override string Size => THK.ToString();
    }
}
