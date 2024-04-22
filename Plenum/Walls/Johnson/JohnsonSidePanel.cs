using ModelTools;
using SolidWorks.Interop.sldworks;
using System.Collections.Generic;
using static Plenum.Plenum;
using aTools = ModelTools.AssemblyTools;
using cTools = ModelTools.ReleaseCOM;
using mTools = Tools.ModelTools;
using bTable = ModelTools.BendTable;
using System.Windows.Media.Imaging;
using System;
using static FileTools.FileTools;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum
{
    // Constructor
    internal abstract class JohnsonSidePanel : Part
    {
        // Static properties
        public static bool Enabled { get; set; }
        internal double THK { get; set; } = SidePanel_THK;
        internal static double LocalLength
        {
            get
            {
                return Length / (MidColumns ? FanCount : 1) - mTools.AssemblyClearance * 2 - mTools.AssemblyClearance + Johnson.ExtraLength;
            }
        }
        private static double HoleToEdge => 3;


        public override RawMaterial Shape => RawMaterial.Plate;
        public override string Size => THK.ToString();

        // Constructor
        protected JohnsonSidePanel(Design callerType) : base(callerType) { }


        // Private methods
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            mTools.EditDimension("HalfLength", "sk:Web", LocalLength / 2 - Johnson.ExtraLength / 2 - mTools.AssemblyClearance / 2, modelDoc2);
            mTools.EditDimension("HalfLengthExtra", "sk:Web", LocalLength / 2 + Johnson.ExtraLength / 2 - mTools.AssemblyClearance / 2, modelDoc2);
            mTools.EditDimension("Height", "sk:Web", PlenumDepth, modelDoc2);
            mTools.EditDimension("THK", "Sheet-Metal", THK, modelDoc2);
            mTools.EditDimension("innerR", "Sheet-Metal", bTable.GetBendRadius(THK), modelDoc2);

            mTools.EditDimension("Hole0", "sk:Hole", CornerAngle.HolePositions[6] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole1", "sk:Hole", CornerAngle.HolePositions[7] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole2", "sk:Hole", CornerAngle.HolePositions[8] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole3", "sk:Hole", CornerAngle.HolePositions[9] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole4", "sk:Hole", CornerAngle.HolePositions[10] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole5", "sk:Hole", PlenumDepth + 1, modelDoc2);
            mTools.EditDimension("Hole6", "sk:Hole", CornerAngle.HolePositions[6] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole7", "sk:Hole", CornerAngle.HolePositions[7] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole8", "sk:Hole", CornerAngle.HolePositions[8] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole9", "sk:Hole", CornerAngle.HolePositions[9] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("Hole10", "sk:Hole", CornerAngle.HolePositions[10] + CornerAngle.YTranslation, modelDoc2);
            mTools.EditDimension("ExtraLength", "sk:Hole", Johnson.ExtraLength - mTools.AssemblyClearance * 2, modelDoc2);

            mTools.EditDimension("Length", "sk:ColumnCut", Length / FanCount - mTools.InterferenceClearance, modelDoc2);
            mTools.EditDimension("FlangeWidth", "sk:ColumnCut", Beam_FlangeWidth + mTools.AssemblyClearance * 2, modelDoc2);
            mTools.EditDimension("PlanBraceZ", "sk:Hole", (Length + Johnson.ExtraLength * 2) / FanCount / 3, modelDoc2);

            SidePanel.EditDimensions_191(modelDoc2);
            SidePanel.EditDimensions_192(modelDoc2);
            SidePanel.EditDimensions_206(modelDoc2);

            mTools.HolePattern(Johnson.ExtraLength - Beam_FlangeWidth / 2 - mTools.AssemblyClearance - mTools.InterferenceClearance - HoleToEdge * 2, out double countExtra, out double spacingExtra);
            mTools.EditDimension("SpacingExtra", "sk:BundleHole", spacingExtra, modelDoc2);
            mTools.EditDimension("CountExtra", "sk:BundleHole", countExtra, modelDoc2);


            mTools.HolePattern(Length / FanCount - Beam_FlangeWidth - mTools.AssemblyClearance * 2 - HoleToEdge * 2, out double count, out double spacing);
            mTools.EditDimension("Spacing", "sk:BundleHole", spacing, modelDoc2);
            mTools.EditDimension("Count", "sk:BundleHole", count, modelDoc2);
        }


    }
}
