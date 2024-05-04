using ModelTools;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;
using static FileTools.FileTools;

namespace Plenum.Structure.Derived
{
    internal class KneeClipBent_R : KneeClipBent
    {
        // Static properties
        public static new bool Enabled => KneeClipBent.Enabled;


        // Constructor
        public KneeClipBent_R(Design callerType) : base(callerType) { }


        // Private methods
        private void StandardClips(ref List<PositionData> pos)
        {
            double xTranslation = -Plenum_Width / 2 + ColumnCenterToPlenumEndClipHole();
            double yTranslation = -Plenum_Depth - BottomOfPlenumToClipHole;
            double zTranslation = EndPanel.CalculateZTranslation() + EndPanel_THK - Leg + Clip_THK / 2;

            pos.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: zTranslation, rY: 180));
            pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: -zTranslation));
        }
        private void LegacyAndJohnsonClips(ref List<PositionData> pos)
        {
            double xTranslation = Plenum_Width / 2;
            double yTranslation = -Plenum_Depth - BottomOfPlenumToClipHole;
            double zTranslation = Plenum_Length / 2 - ColumnCenterToPlenumSideClipHole();

            pos.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: zTranslation, rY: -90));
            pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: -zTranslation, rY: 90));

            if (BraceType.Contains("L"))
            {
                if (Mid_Columns)
                {
                    for (int i = 0; i < Fan_Count - 1; i++)
                    {
                        double z = zTranslation;
                        z -= Plenum_Length / Fan_Count;
                        pos.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: z, rY: -90));
                    }

                    for (int i = 0; i < Fan_Count - 1; i++)
                    {
                        double z = zTranslation;
                        z -= Plenum_Length / Fan_Count;
                        pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: -z, rY: 90));
                    }
                }
            }
        }
        private void CenterPanelClips(ref List<PositionData> pos)
        {
            double xTranslation = Plenum_Width / 2 - ColumnCenterToPlenumEndClipHole();
            double yTranslation = -Plenum_Depth - BottomOfPlenumToClipHole;
            double zTranslation = Plenum_Length / 2;

            for (int i = 1; i < Fan_Count; i++)
            {
                zTranslation -= Plenum_Length / Fan_Count;
                pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, rY: 180));
            }
        }


        // Method overrides
        public override string StaticPartNo => "144";
        public override RawMaterial Shape => RawMaterial.Plate;
        public override string Size => Clip_THK.ToString();
        protected override AssemblyDoc ParentAssembly => Plenum.AssemblyDoc;
        public override List<PositionData> Position
        {
            get
            {
                var pos = new List<PositionData>();

                if (BraceType.Contains("L"))
                    CenterPanelClips(ref pos);

                if (PlenumDesign == Design.Standard)
                    StandardClips(ref pos);

                else
                    LegacyAndJohnsonClips(ref pos);
            

                return pos;
            }
        }

    }
}
