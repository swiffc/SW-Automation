using ModelTools;
using SolidWorks.Interop.sldworks;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;
using static FileTools.FileTools;
using mTools = Tools.ModelTools;

namespace Plenum.Structure
{
    internal class KneeClipFlat : Part
    {
        // Static properties
        public static bool Enabled
        {
            get
            {
                return
                    (
                        PlenumDesign == Design.Standard
                        &&
                        (BraceType.Contains("L") || BraceType.Contains("T"))
                    )

                    ||

                    PlenumDesign == Design.Legacy && BraceType.Contains("L")

                    ? true : false;
            }
        }


        // Constructor
        public KneeClipFlat(Design callerType) : base(callerType) { }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            mTools.EditDimension("Angle", "sk:Plate", BraceAngle, modelDoc2);
            mTools.EditDimension("FlangeGage", "sk:Plate", BraceType.Contains("T") ? WT_FlangeGage : 0.001, modelDoc2);
            mTools.EditDimension("THK", "Plate", Clip_THK, modelDoc2);
            mTools.EditDimension("Diameter", "sk:Hole", HoleDiameter_Structural, modelDoc2);
        }
        protected override void FeatureSuppression(ModelDoc2 modelDoc2)
        {
            mTools.SuppressFeatures(BraceType.Contains("L") ? false : true, modelDoc2, "Slot");
        }


        // Property overrides
        public override string StaticPartNo => "146";
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
                    double xTranslation = SidePanel.CalculateXTranslation() + SidePanel_THK;
                    double yTranslation = -PlenumDepth - BottomOfPlenumToClipHole;
                    double zTranslation = Length / 2 - ColumnCenterToPlenumSideClipHole();

                    pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation));
                    pos.Add(PositionData.Create(tX: -xTranslation - Clip_THK, tY: yTranslation, tZ: zTranslation));
                    pos.Add(PositionData.Create(tX: xTranslation + Clip_THK, tY: yTranslation, tZ: -zTranslation, rY: 180));
                    pos.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: -zTranslation, rY: 180));


                    if (MidColumns)
                    {
                        for (int i = 0; i < FanCount - 1; i++)
                        {
                            double z = zTranslation;
                            z -= Length / FanCount;
                            pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: z));
                            pos.Add(PositionData.Create(tX: -xTranslation - Clip_THK, tY: yTranslation, tZ: z));
                        }

                        for (int i = 0; i < FanCount - 1; i++)
                        {
                            double z = zTranslation;
                            z -= Length / FanCount;
                            pos.Add(PositionData.Create(tX: xTranslation + Clip_THK, tY: yTranslation, tZ: -z, rY: 180));
                            pos.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: -z, rY: 180));
                        }
                    }
                }
                if (PlenumDesign == Design.Legacy && BraceType.Contains("L"))
                {
                    double xTranslation = Width / 2 - ColumnCenterToPlenumEndClipHole();
                    double yTranslation = -PlenumDepth - BottomOfPlenumToClipHole;
                    double zTranslation = EndPanel.CalculateZTranslation() + EndPanel_THK;

                    pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation + Clip_THK, rY: -90));
                    pos.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: zTranslation, rY: 90));
                    pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: -zTranslation, rY: -90));
                    pos.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: -zTranslation - Clip_THK, rY: 90));
                }

                return pos;
            }
        }
    }
}
