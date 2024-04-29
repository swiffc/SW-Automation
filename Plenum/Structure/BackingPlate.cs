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

namespace Plenum.Structure
{
    internal class BackingPlate : Part
    {
        // Static properties
        public static bool Enabled => KneeClipBent.Enabled;
        static public double FloorGage
        {
            get
            {
                if (PlenumDesign == Design.Standard)
                {
                    return EndPanel.Gauge - EndPanel_THK;
                }
                else // Legacy
                {
                    return SidePanel.Gauge - SidePanel_THK;
                }
            }
        }


        // Constructor
        public BackingPlate(Design callerType) : base(callerType) { }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            EditDimension("Gage", "sk:FloorHoles", FloorGage, modelDoc2);
            EditDimension("Width", "Plate", KneeClipBent.GetWidth(out _, out _), modelDoc2);
        }


        // Property overrides
        public override string StaticPartNo => "140";
        public override RawMaterial Shape => RawMaterial.Plate;
        public override string Size => "0.25";
        protected override AssemblyDoc ParentAssembly => Plenum.AssemblyDoc;
        public override List<PositionData> Position
        {
            get
            {
                KneeClipBent.GetWidth(out _, out double holeToPlateCenter);
                double yTranslation = -Plenum_Depth + Math.Max(EndPanel_THK, SidePanel_THK) + FloorPanel.THK;

                if (PlenumDesign == Design.Standard)
                {
                    double xTranslation = -Plenum_Width / 2 + ColumnCenterToPlenumEndClipHole() + holeToPlateCenter;
                    double zTranslation = Plenum_Length / 2 + Beam_Depth / 2;

                    return new List<PositionData>
                    {
                        PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation),
                        PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: zTranslation),

                        PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: -zTranslation, rY: 180),
                        PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: -zTranslation, rY: 180),
                    };
                }
                else // Legacy
                {
                    double xTranslation = Plenum_Width/2 + Beam_Depth/2 - Beam_FlangeTHK - SidePanel_THK;
                    double zTranslation = Plenum_Length/2 - ColumnCenterToPlenumSideClipHole() - holeToPlateCenter;

                    var pos =  new List<PositionData>
                    {
                        PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, rY: -90),
                        PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: -zTranslation, rY: -90),

                        PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: zTranslation, rY: 90),
                        PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: -zTranslation, rY: 90),
                    };

                    if (Mid_Columns && BraceType.Contains("L"))
                    {
                        for (int i = 0; i < Fan_Count - 1; i++)
                        {
                            double z = zTranslation;
                            z -= Plenum_Length / Fan_Count;
                            pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: z, rY: -90));
                            pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: -z, rY: -90));
                        }

                        for (int i = 0; i < Fan_Count - 1; i++)
                        {
                            double z = zTranslation;
                            z -= Plenum_Length / Fan_Count;
                            pos.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: z, rY: 90));
                            pos.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: -z, rY: 90));
                        }
                    }

                    return pos;
                }
            }
        }


    }
}
