using ModelTools;
using SolidWorks.Interop.sldworks;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;
using static FileTools.FileTools;
using static Tools.ModelTools;

namespace Plenum.Structure
{
    internal class KneeClipFlat : Part
    {
        // Static properties
        public static bool Enabled
        {
            get
            {
                if (PlenumDesign != Design.Johnson)
                {
                    if (BraceType.Contains("L") || BraceType.Contains("T"))
                    {
                        return true;
                    }
                }
                return false;
            }
        }
        static public double Gage
        {
            get
            {
                return BraceType.Contains("L") ? 2.5 : WT_FlangeGage;
            }
        }
        static public double FlangeGage
        {
            get
            {
                return BraceType.Contains("T") ? WT_FlangeGage : 0.001;
            }
        }


        // Constructor
        public KneeClipFlat(Design callerType) : base(callerType) { }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            EditDimension("Angle", "sk:Plate", BraceAngle, modelDoc2);
            EditDimension("FlangeGage", "sk:Plate", FlangeGage, modelDoc2);
            EditDimension("THK", "Plate", Clip_THK, modelDoc2);
            EditDimension("Gage", "sk:Plate", Gage, modelDoc2);
            EditDimension("Diameter", "sk:Hole", HoleDiameter_Structural, modelDoc2);
        }
        protected override void FeatureSuppression(ModelDoc2 modelDoc2)
        {
            SuppressFeatures(BraceType.Contains("L") ? false : true, modelDoc2, "Slot");
        }


        // Static methods
        public static double GetReferenceHoleToPlateEnd()
        {
            // Viewing YZ plane at the part level
            AAS(BraceAngle, out _, FlangeGage / 2, out double zFromOriginToWorkLine);
            AAS(BraceAngle, out double zFromOriginToWorkLine_zWorkHole, PlenumBoundsToHole * 2, out _);
            double workHoletoRightHole = 3;
            double zOriginaToRightSideOfPlate = zFromOriginToWorkLine + zFromOriginToWorkLine_zWorkHole + workHoletoRightHole + HoleToEdge;

            return zOriginaToRightSideOfPlate;
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
                    double yTranslation = -Plenum_Depth - BottomOfPlenumToClipHole;
                    double zTranslation = Plenum_Length / 2 - ColumnCenterToPlenumSideClipHole();

                    pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation));
                    pos.Add(PositionData.Create(tX: -xTranslation - Clip_THK, tY: yTranslation, tZ: zTranslation));
                    pos.Add(PositionData.Create(tX: xTranslation + Clip_THK, tY: yTranslation, tZ: -zTranslation, rY: 180));
                    pos.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: -zTranslation, rY: 180));


                    if (Mid_Columns && BraceType.Contains("L"))
                    {
                        for (int i = 0; i < Fan_Count - 1; i++)
                        {
                            double z = zTranslation;
                            z -= Plenum_Length / Fan_Count;
                            pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: z));
                            pos.Add(PositionData.Create(tX: -xTranslation - Clip_THK, tY: yTranslation, tZ: z));
                        }

                        for (int i = 0; i < Fan_Count - 1; i++)
                        {
                            double z = zTranslation;
                            z -= Plenum_Length / Fan_Count;
                            pos.Add(PositionData.Create(tX: xTranslation + Clip_THK, tY: yTranslation, tZ: -z, rY: 180));
                            pos.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: -z, rY: 180));
                        }
                    }
                }
                else
                {
                    double xTranslation = Plenum_Width / 2 - ColumnCenterToPlenumEndClipHole();
                    double yTranslation = -Plenum_Depth - BottomOfPlenumToClipHole;
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
