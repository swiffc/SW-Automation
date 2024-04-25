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

                if (PlenumDesign == Design.Standard)
                {
                    double xTranslation = - Width / 2 + ColumnCenterToPlenumEndClipHole();
                    double yTranslation = -PlenumDepth - BottomOfPlenumToClipHole;
                    double zTranslation = EndPanel.CalculateZTranslation() + EndPanel_THK - Leg + Clip_THK/2;

                    pos.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: zTranslation, rY: 180));
                    pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: -zTranslation));
                }

                else
                {
                    double xTranslation = Width /2 ;
                    double yTranslation = -PlenumDepth - BottomOfPlenumToClipHole;
                    double zTranslation = Length / 2 - ColumnCenterToPlenumSideClipHole();

                    pos.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: zTranslation, rY: -90));
                    pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: -zTranslation, rY: 90));

                    if (MidColumns && BraceType.Contains("L"))
                    {
                        for (int i = 0; i < FanCount - 1; i++)
                        {
                            double z = zTranslation;
                            z -= Length / FanCount;
                            pos.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: z, rY: -90));
                        }

                        for (int i = 0; i < FanCount - 1; i++)
                        {
                            double z = zTranslation;
                            z -= Length / FanCount;
                            pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: -z, rY: 90));
                        }
                    }
                }
            

                return pos;
            }
        }

    }
}
