using FileTools.CommonData;
using ModelTools;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.FileTools;
using static FileTools.CommonData.CommonData;
using static Tools.ModelTools;
using static ModelTools.BendTable;
using Plenum.Floor;
using Plenum.Helpers.Static;
using static FileTools.Properties.Settings;

namespace Plenum.Structure
{
    internal class WallStiffener : Part
    {
        // Static properties
        static public bool Enabled => true;
        static public double Gage => 1.75;
        static public double Legs => 3;


        // Constructor
        public WallStiffener(Design callerType) : base(callerType) { }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            EditDimension("Length", "L", CornerAngle.LocalLength, modelDoc2);
        }


        // Private methods
        private void AddEndStiffenersAtBraceClips(ref List<PositionData> pos)
        {
            double bentClipWidth = KneeClipBent.GetWidth(out _, out double holeToPlateCenter);
            double xTranslation = -Plenum_Width / 2 + ColumnCenterToPlenumEndClipHole() + holeToPlateCenter + bentClipWidth / 2 + Clearance;

            var endBraceStiffeners = new List<PositionData>
            {
                PositionData.Create(tX:xTranslation, tY: YTranslation_AllBraces, tZ: ZTranslation_EndPanelBrace, rZ: 180),
                PositionData.Create(tX: -xTranslation, tY: YTranslation_AllBraces, tZ: ZTranslation_EndPanelBrace),

                PositionData.Create(tX: xTranslation, tY: YTranslation_AllBraces, tZ: -ZTranslation_EndPanelBrace, rY: 180),
                PositionData.Create(tX: -xTranslation, tY: YTranslation_AllBraces, tZ: -ZTranslation_EndPanelBrace, rY: 180, rZ: 180),
            };
            pos.AddRange(endBraceStiffeners);
        }
        private void AddDividerStiffenersAtBraceClips(ref List<PositionData> pos)
        {
            double bentClipWidth = KneeClipBent.GetWidth(out _, out double holeToPlateCenter);
            double xTranslation = -Plenum_Width / 2 + ColumnCenterToPlenumEndClipHole() + holeToPlateCenter + bentClipWidth / 2 + Clearance;
            double z = Plenum_Length / 2 + DividerPanel.THK;

            for (int i = 1; i < Fan_Count; i++)
            {
                z -= Plenum_Length / Fan_Count;
                pos.Add(PositionData.Create(tX: xTranslation, tY: YTranslation_AllBraces, tZ: z, rY: 180));
                pos.Add(PositionData.Create(tX: -xTranslation, tY: YTranslation_AllBraces, tZ: z, rY: 180, rZ: 180));
            }
        }
        private void AddEndStiffenersEvenlySpread(ref List<PositionData> pos)
        {
            double stiffenerCount = EndStiffenerCount;
            double xTranslation = -Plenum_Width / 2 + Plenum_Width / (stiffenerCount + 1) - Gage;
            bool isFirstStiffenerToBeFlipped = false;

            for (int i = 0; i < stiffenerCount; i++)
            {
                double zRotation = i > stiffenerCount / 2 ? 0 : 180;
                if (i > stiffenerCount / 2 && !isFirstStiffenerToBeFlipped)
                {
                    xTranslation += Gage * 2;
                    isFirstStiffenerToBeFlipped = true;
                }
                pos.Add(PositionData.Create(tX: xTranslation, tY: YTranslation_AllBraces, tZ: ZTranslation_EndPanelBrace, rZ: zRotation));
                pos.Add(PositionData.Create(tX: xTranslation, tY: YTranslation_AllBraces, tZ: -ZTranslation_EndPanelBrace, rX: 180, rZ: zRotation));
                xTranslation += Plenum_Width / (stiffenerCount + 1);
            }
        }
        private void AddDividerStiffenersEvenlySpread(ref List<PositionData> pos)
        {
            double stiffenerCount = DividerStiffenerCount;
            double xTranslation = -Plenum_Width / 2 + Plenum_Width / (stiffenerCount + 1) - Gage;
            bool isFirstStiffenerToBeFlipped = false;
            double z = 0;
            switch (PlenumDesign)
            {
                case Design.Standard:
                    z = ZTranslation_EndPanelBrace + DividerPanel.THK * 0.5 - Beam_Depth / 2;
                    break;
                case Design.Johnson:
                    z = ZTranslation_EndPanelBrace - Johnson_ExtraLength + EndPanel_THK * 0.5;
                    break;
                case Design.Legacy:
                    z = ZTranslation_EndPanelBrace + DividerPanel.THK * 1.5;
                    break;
            }
            double savedX = xTranslation;

            // for each center pannel
            for (int j = 1; j < Fan_Count; j++)
            {
                z -= Plenum_Length / Fan_Count;
                //z -= PlenumDesign == Design.Johnson ? (Default.Johnson_ExtraLength + EndPanel_THK) : 0;
                for (int i = 0; i < stiffenerCount; i++) // for each stiffener on a panel
                {
                    double zRotation = i > stiffenerCount / 2 ? 0 : 180;
                    if (i > stiffenerCount / 2 && !isFirstStiffenerToBeFlipped)
                    {
                        xTranslation += Gage * 2;
                        isFirstStiffenerToBeFlipped = true;
                    }
                    pos.Add(PositionData.Create(tX: xTranslation, tY: YTranslation_AllBraces, tZ: z, rZ: zRotation, rX: 180));
                    xTranslation += Plenum_Width / (stiffenerCount + 1);
                }
                xTranslation = savedX;
            }
        }
        private void AddSideStiffenersAtBraceClips(ref List<PositionData> pos)
        {
            double zTranslation = Plenum_Length / 2 - ColumnCenterToPlenumSideClipHole() - KneeClipFlat.GetReferenceHoleToPlateEnd() - Clearance;

            var sideBraceStiffeners = new List<PositionData>
            {
                PositionData.Create(tX: XTranslation_SidePanelBrace, tY: YTranslation_AllBraces, tZ: zTranslation, rY: 90, rX: 180),
                PositionData.Create(tX: -XTranslation_SidePanelBrace, tY: YTranslation_AllBraces, tZ: zTranslation, rY: 90),

                PositionData.Create(tX: XTranslation_SidePanelBrace, tY: YTranslation_AllBraces, tZ: -zTranslation, rY: -90),
                PositionData.Create(tX: -XTranslation_SidePanelBrace, tY: YTranslation_AllBraces, tZ: -zTranslation, rY: -90, rX: 180),
            };
            pos.AddRange(sideBraceStiffeners);
            if (Mid_Columns && BraceType.Contains("L"))
            {
                for (int i = 0; i < Fan_Count - 1; i++)
                {
                    double z = zTranslation;
                    z -= Plenum_Length / Fan_Count;
                    pos.Add(PositionData.Create(tX: XTranslation_SidePanelBrace, tY: YTranslation_AllBraces, tZ: z, rY: 90, rX: 180));
                    pos.Add(PositionData.Create(tX: -XTranslation_SidePanelBrace, tY: YTranslation_AllBraces, tZ: z, rY: 90));
                }

                for (int i = 0; i < Fan_Count - 1; i++)
                {
                    double z = zTranslation;
                    z -= Plenum_Length / Fan_Count;

                    pos.Add(PositionData.Create(tX: XTranslation_SidePanelBrace, tY: YTranslation_AllBraces, tZ: -z, rY: -90));
                    pos.Add(PositionData.Create(tX: -XTranslation_SidePanelBrace, tY: YTranslation_AllBraces, tZ: -z, rY: -90, rX: 180));
                }
            }
        }
        private void AddSideStiffenersAtMachineryMount(ref List<PositionData> pos)
        {
            var zTranslations = FanCenter.ZTranslation(PlenumDesign);
            double mmHoleToHole = MachineryMount_Width / 2 - HoleClosestToEdge_To_WidthBoundary + Gage;

            for (int i = 0; i < Fan_Count; i++)
            {
                double zTranslation = zTranslations[i] + mmHoleToHole;
                pos.Add(PositionData.Create(tX: XTranslation_SidePanelBrace, tY: YTranslation_AllBraces, tZ: zTranslation, rY: 90, rX: 180));
                pos.Add(PositionData.Create(tX: -XTranslation_SidePanelBrace, tY: YTranslation_AllBraces, tZ: zTranslation, rY: 90));

                pos.Add(PositionData.Create(tX: XTranslation_SidePanelBrace, tY: YTranslation_AllBraces, tZ: -zTranslation, rY: -90));
                pos.Add(PositionData.Create(tX: -XTranslation_SidePanelBrace, tY: YTranslation_AllBraces, tZ: -zTranslation, rY: -90, rX: 180));

                if (!BraceType.Contains("L"))
                {
                    double totalSpan = Plenum_Length / Fan_Count;
                    double targetSpan = (totalSpan - mmHoleToHole) / 2;
                    double z = zTranslation + targetSpan / 2;
                    pos.Add(PositionData.Create(tX: XTranslation_SidePanelBrace, tY: YTranslation_AllBraces, tZ: z, rY: 90, rX: 180));
                    pos.Add(PositionData.Create(tX: -XTranslation_SidePanelBrace, tY: YTranslation_AllBraces, tZ: z, rY: 90));

                    pos.Add(PositionData.Create(tX: XTranslation_SidePanelBrace, tY: YTranslation_AllBraces, tZ: -z, rY: -90));
                    pos.Add(PositionData.Create(tX: -XTranslation_SidePanelBrace, tY: YTranslation_AllBraces, tZ: -z, rY: -90, rX: 180));
                }
            }


        }


        // Private properties
        private double XTranslation_SidePanelBrace => SidePanel.CalculateXTranslation() - SidePanel_THK;
        private double YTranslation_AllBraces => -Plenum_Depth / 2;
        private double ZTranslation_EndPanelBrace
        {
            get
            {
                switch (PlenumDesign)
                {
                    case Design.Standard:
                        return Plenum_Length / 2 + Beam_Depth / 2;
                    case Design.Johnson:
                        return Plenum_Length / 2 + Default.Johnson_ExtraLength;
                    case Design.Legacy:
                        return Plenum_Length / 2 - EndPanel_THK;
                }
                throw new NotImplementedException();
            }
        }
        private double Clearance => 0.5;


        // Property overrides
        public override string StaticPartNo => "186S";
        public override RawMaterial Shape => RawMaterial.Angle;
        public override string Size => "L3x3x0.1875";
        protected override AssemblyDoc ParentAssembly => Plenum.AssemblyDoc;
        public override List<PositionData> Position
        {
            get
            {
                var pos = new List<PositionData>();

                AddSideStiffenersAtMachineryMount(ref pos);

                if (BraceType.Contains("L"))
                {
                    AddEndStiffenersAtBraceClips(ref pos);
                    AddSideStiffenersAtBraceClips(ref pos);
                    AddDividerStiffenersAtBraceClips(ref pos);
                }
                else
                {
                    AddEndStiffenersEvenlySpread(ref pos);
                    AddDividerStiffenersEvenlySpread(ref pos);
                }


                return pos;
            }
        }
    }
}
