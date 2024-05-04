using FileTools.CommonData;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using static Tools.ModelTools;
using static ModelTools.BendTable;

namespace Plenum.Structure
{
    internal abstract class KneeClipBent : Part
    {
        // Static properties
        static public double FlangeGage => Leg - SidePanel.Gauge;
        static public double FaceGage => 2.5;
        static public double Leg
        {
            get
            {
                if (PlenumDesign == Design.Standard)
                {
                    return EndPanel_THK + Beam_Depth / 2 + Clip_THK / 2;
                }
                else if (PlenumDesign == Design.Legacy)
                {
                    return Beam_Depth / 2 - Beam_FlangeTHK + Clip_THK / 2;
                }
                else // Johnson
                {
                    return Beam_Depth / 2 + SidePanel_THK + Clip_THK / 2;
                }
            }
        }
        static public double SlotWidth => 1.5;
        public static bool Enabled
        {
            get
            {
                return BraceType.Contains("L") ||
                    (BraceType.Contains("T") && PlenumDesign != Design.Standard) ? true : false;
            }
        }


        // Constructor
        protected KneeClipBent(Design callerType) : base(callerType) { }


        // Public methods
        public static double GetWidth(out double positiveX, out double holeToPlateCenter)
        {
            AAS(BraceAngle, out double xHoleToSlot, out _, FaceGage);

            double diagSlotTrueCenterToSlotSideCenter = (SlotWidth - HoleDiameter_Structural) / 2;
            AAS(BraceAngle, out _, out double xSlotTrueCenterToSlotSideCenter, diagSlotTrueCenterToSlotSideCenter);


            double negativeX = xHoleToSlot + xSlotTrueCenterToSlotSideCenter + HoleToEdge;

            AAS(BraceAngle, out holeToPlateCenter, PlenumBoundsToHole, out _);
            positiveX = holeToPlateCenter * 2 + negativeX;

            return positiveX + negativeX;
        }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            EditDimension("Diameter", "sk:Plate", HoleDiameter_Structural, modelDoc2);
            EditDimension("Angle", "sk:Plate", BraceAngle, modelDoc2);

            EditDimension("Gage", "sk:FlangeHoles", FlangeGage, modelDoc2);
            EditDimension("Leg", "sk:Flange", Leg, modelDoc2);
            EditDimension("THK", "Sheet-Metal", Clip_THK, modelDoc2);
            EditDimension("R", "FlangeR", GetBendRadius(Clip_THK), modelDoc2);

        }


        // Private methods
        private static double FlangeModifier()
        {
            double panelTHK = PlenumDesign == Design.Standard ? EndPanel_THK : SidePanel_THK;

            switch (panelTHK)
            {
                case 0.1344:
                    return 0.1875;
                case 0.1875:
                    return 0.125;
                case 0.375:
                    return -0.3125;
                default:
                    return 0;
            }
        }
    }
}
