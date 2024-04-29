using ModelTools;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Plenum.Plenum;
using mTools = Tools.ModelTools;
using bTools = ModelTools.BendTable;
using Plenum.Helpers.Static;
using Plenum.Floor.Derived.Derived;
using Plenum.Floor.Derived;
using static FileTools.FileTools;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;
using static FileTools.Properties.Settings;

namespace Plenum.Floor
{
    internal abstract class FloorExtension : Part
    {
        public FloorExtension(Design callerType) : base(callerType) { }
        public override RawMaterial Shape => RawMaterial.Plate;
        public override string Size => FloorPanel.THK.ToString();
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            double width = FloorPanel.GetWidth(CallerType) * 2 + mTools.AssemblyClearance;
            mTools.EditDimension("Width", "sk:Plate", width, modelDoc2);

            mTools.EditDimension("SideGauge", "sk:FloorHole", SidePanel.Gauge - SidePanel_THK - SidePanel.R, modelDoc2);

            mTools.HolePattern(width / 2 - 1.5 * 2 - mTools.AssemblyClearance / 2, out double count1, out double spacing1);
            mTools.EditDimension("Count", "sk:FlangeHole", count1, modelDoc2);
            mTools.EditDimension("Spacing", "sk:FlangeHole", spacing1, modelDoc2);

            double span = CallerType == Design.Johnson ? DividerPanel.LocalWidth / 2 - mTools.HoleToEdge_General * 3 : EndPanel.LocalWidth / 2 - Beam_FlangeWidth / 2 - mTools.HoleToEdge_General * 3 - mTools.AssemblyClearance;
            mTools.HolePattern(span, out double count2, out double spacing2);
            mTools.EditDimension("Count", "sk:FloorHole", count2, modelDoc2);
            mTools.EditDimension("Spacing", "sk:FloorHole", spacing2, modelDoc2);

            double sideFrameLength = CallerType == Design.Johnson ? JohnsonSidePanel.LocalLength - Beam_FlangeWidth/2 - 1.5:  SidePanel.LocalLength;
            mTools.HolePattern(sideFrameLength / 2 - (FloorSplice.NominalLength / 2 + InnerFloorPanel.GetLength()) - 3 * 2, out double count3, out double spacing3);
            mTools.EditDimension("LengthSpacing", "sk:FloorHole", spacing3, modelDoc2);
            mTools.EditDimension("LengthCount", "sk:FloorHole", count3, modelDoc2);

            if (CallerType != Design.Johnson)
            {
                mTools.EditDimension("X", "sk:ColumnCut", Plenum_Width / 2, modelDoc2);
                mTools.EditDimension("Z", "sk:ColumnCut", (Plenum_Length + (CallerType == Design.Johnson ? Default.Johnson_ExtraLength * 2 : 0)) / Fan_Count / 2 - InnerFloorPanel.GetLength() - FloorSplice.NominalLength / 2 - mTools.AssemblyClearance, modelDoc2);
                mTools.EditDimension("Depth", "sk:ColumnCut", Beam_Depth / 2 + mTools.AssemblyClearance, modelDoc2);
                mTools.EditDimension("FlangeWidth", "sk:ColumnCut", Beam_FlangeWidth / 2 + mTools.AssemblyClearance, modelDoc2);
                mTools.EditDimension("WebTHK", "sk:ColumnCut", Beam_WebTHK / 2 + mTools.AssemblyClearance, modelDoc2);
                mTools.EditDimension("FlangeTHK", "sk:ColumnCut", Beam_FlangeTHK / 2 + mTools.AssemblyClearance, modelDoc2);
            }



        }
    }
}
