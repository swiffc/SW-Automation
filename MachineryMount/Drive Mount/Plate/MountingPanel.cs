using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FileTools.Base;
using ModelTools;
using FileTools.CommonData;
using static Tools.ModelTools;
using static FileTools.CommonData.CommonData;

namespace MachineryMount.Drive_Mount.Plate
{
    class MountingPanel : Part
    {
        // Static properties
        static internal double HoleClosestToEdge_To_WidthBoundary => CommonData.HoleClosestToEdge_To_WidthBoundary;
        static internal double HoleCount_PlenumSidePanel
        {
            get
            {
                HolePattern_MountingPanelToPlenumSidePanel(out double count, out _);
                return count;
            }
        }
        static internal double HoleSpacing_PlenumSidePanel
        {
            get
            {
                HolePattern_MountingPanelToPlenumSidePanel(out _, out double spacing);
                return spacing;
            }
        }


        // Private methods
        static private void HolePattern_MountingPanelToPlenumSidePanel(out double count, out double spacing)
        {
            HolePattern(MachineryMount_Width - HoleClosestToEdge_To_WidthBoundary * 2, out count, out spacing, 12);
        }








        public MountingPanel(SubAssembly parentSubAssembly) : base(parentSubAssembly)
        {
        }

        public override bool Enabled => throw new NotImplementedException();

        public override string StaticPartNo => throw new NotImplementedException();

        public override Shape RawMaterialShape => throw new NotImplementedException();

        public override string SizeOrThickness => throw new NotImplementedException();

        public override List<PositionData> Position => throw new NotImplementedException();
    }
}
