using ModelTools;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;
using static FileTools.FileTools;
using static FileTools.Properties.Settings;

namespace Plenum.Structure.Derived
{
    internal class KneeClipBent_L : KneeClipBent
    {
        // Static properties
        public static new bool Enabled => KneeClipBent.Enabled;


        // Constructor
        public KneeClipBent_L(Design callerType) : base(callerType) { }


        // Property overrides
        public override string StaticPartNo => "142";
        public override RawMaterial Shape => RawMaterial.Plate;
        public override string Size => Default.Clip_THK.ToString();
        protected override AssemblyDoc ParentAssembly => Plenum.AssemblyDoc;
        public override List<PositionData> Position
        {
            get
            {
                var pos = new List<PositionData>();

                if (PlenumDesign == Design.Standard)
                {
                    double xTranslation = Plenum_Width / 2 - ColumnCenterToPlenumEndClipHole();
                    double yTranslation = -Plenum_Depth - BottomOfPlenumToClipHole;
                    double zTranslation = EndPanel.CalculateZTranslation() + EndPanel_THK - Leg + Clip_THK / 2;

                    pos.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: zTranslation));
                    pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: -zTranslation, rY: 180));
                }

                else
                {
                    double xTranslation = Plenum_Width / 2;
                    double yTranslation = -Plenum_Depth - BottomOfPlenumToClipHole;
                    double zTranslation = Plenum_Length / 2 - ColumnCenterToPlenumSideClipHole();

                    pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, rY: -90));
                    pos.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: -zTranslation, rY: 90));

                    if (Mid_Columns && BraceType.Contains("L"))
                    {
                        for (int i = 0; i < Fan_Count - 1; i++)
                        {
                            double z = zTranslation;
                            z -= Plenum_Length / Fan_Count;
                            pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: z, rY: -90));
                        }

                        for (int i = 0; i < Fan_Count - 1; i++)
                        {
                            double z = zTranslation;
                            z -= Plenum_Length / Fan_Count;
                            pos.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: -z, rY: 90));
                        }
                    }
                }

                return pos;
            }
        }


    }
}
