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
        static public double Gage
        {
            get
            {
                if (PlenumDesign == Design.Standard)
                {
                    return Leg - EndPanel.Gauge;
                }
                else if (PlenumDesign == Design.Legacy)
                {
                    return Leg - SidePanel.Gauge;
                }
                else
                {
                    return 1.5;
                }
            }
        }
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
                else
                {
                    return 3;
                }
            }
        }


        // Constructor
        protected KneeClipBent(Design callerType) : base(callerType) { }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            EditDimension("Gage", "sk:FlangeHoles", Gage, modelDoc2);
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
