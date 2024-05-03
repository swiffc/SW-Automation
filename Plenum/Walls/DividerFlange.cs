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
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum.Walls
{
    internal class DividerFlange : Part
    {
        // Static properties
        public static bool Enabled
        {
            get
            {
                return Fan_Count > 1 ? true : false;
            }
        }
        internal static double THK => DividerPanel.THK;
        internal static double Flange => 3;


        // Constructor
        public DividerFlange(Design callerType) : base(callerType) { }


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
            double valueLimit = Beam_Depth / 2 - Beam_FlangeTHK - mTools.InterferenceClearance;
            if ((CallerType != Design.Standard && Mid_Columns))
                mTools.EditDimension("Width", "sk:ColumnSeal", maxValue, modelDoc2);
            else if (maxValue > valueLimit)
                mTools.EditDimension("Width", "sk:ColumnSeal", valueLimit, modelDoc2);
            else
                mTools.EditDimension("Width", "sk:ColumnSeal", maxValue, modelDoc2);

            mTools.EditDimension("Extension", "ColumnSeal", (CallerType == Design.Standard ? Beam_FlangeWidth / 2 : Beam_Depth / 2) - mTools.AssemblyClearance, modelDoc2);

            DividerPanel.EditDimensions_WebHoles(modelDoc2);

            mTools.EditDimension("Gauge", "sk:BottomHole", THK + bTable.GetBendRadius(THK) + FloorPanel.HoleToEdge1, modelDoc2);
        }
        protected override void FeatureSuppression(ModelDoc2 modelDoc2)
        {
            bool suppress = true;
            if (CallerType == Design.Standard && Mid_Columns)
                suppress = false;
            else if (CallerType == Design.Legacy && Mid_Columns)
                suppress = false;
            else if (CallerType == Design.Johnson && Mid_Columns)
                suppress = false;

            mTools.SuppressFeatures(suppress, modelDoc2, "ColumnSeal", "1", "2", "3");
        }


        // Private properties
        private double Length
        {
            get
            {
                double length;
                if (CallerType == Design.Standard)
                    if (Mid_Columns)
                        length = Plenum_Width - Beam_FlangeWidth - mTools.InterferenceClearance * 2;
                    else
                        length = Plenum_Width - CornerAngle.Leg * 2 - mTools.AssemblyClearance * 2;

                else if (CallerType == Design.Johnson && !Mid_Columns)
                    length = Plenum_Width + Beam_Depth - CornerAngle.Leg * 2 - mTools.InterferenceClearance * 2;
                else if (CallerType == Design.Johnson && Mid_Columns)
                    length = Plenum_Width - Beam_Depth - DividerAngle.LongLeg * 2 - mTools.AssemblyClearance * 3;

                else if (CallerType == Design.Legacy && !Mid_Columns)
                    length = Plenum_Width + Beam_Depth - Beam_FlangeTHK * 2 - SidePanel_THK * 2 - CornerAngle.Leg * 2 - mTools.AssemblyClearance * 2;
                else if (CallerType == Design.Legacy && Mid_Columns)
                    length = Plenum_Width - Beam_Depth - DividerAngle.LongLeg * 2 - mTools.AssemblyClearance * 3;

                else
                    length = Plenum_Width - Beam_Depth - mTools.AssemblyClearance * 4;
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

                double zTranslation = Plenum_Length / 2 - EndPanel_THK / 2;


                List<PositionData> _position = new List<PositionData>();

                if (Fan_Count > 1)
                {
                    for (int i = 1; i < Fan_Count; i++)
                    {
                        zTranslation -= Plenum_Length / Fan_Count;
                        _position.Add(PositionData.Create(tZ: zTranslation, tY: -Plenum_Depth));
                    }
                }

                return _position;

            }
        }
        public override RawMaterial Shape => RawMaterial.Plate;
        public override string Size => THK.ToString();
    }
}
