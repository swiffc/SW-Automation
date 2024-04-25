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
    internal class WallStiffener : Part
    {
        // Static properties
        static public bool Enabled => KneeClipBent.Enabled;


        // Constructor
        public WallStiffener(Design callerType) : base(callerType) { }


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

                if (PlenumDesign == Design.Standard)
                {
                    double yTranslation = -PlenumDepth / 2;
                    double clearance = 0.5;

                    // Side panel brace stiffeners
                    if (BraceType.Contains("L") || BraceType.Contains("T"))
                    {
                        double xTranslation = SidePanel.CalculateXTranslation() - SidePanel_THK;
                        double zTranslation = Length / 2 - ColumnCenterToPlenumSideClipHole() - KneeClipFlat.GetReferenceHoleToPlateEnd() - clearance;

                        var sideBraceStiffeners = new List<PositionData>
                        {
                            PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, rY: 90, rX:180),
                            PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: zTranslation, rY: 90),

                            PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: -zTranslation, rY: -90),
                            PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: -zTranslation, rY: -90, rX:180),
                        };
                        pos.AddRange(sideBraceStiffeners);
                        if (MidColumns && BraceType.Contains("L"))
                        {
                            for (int i = 0; i < FanCount - 1; i++)
                            {
                                double z = zTranslation;
                                z -= Length / FanCount;
                                pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: z, rY: 90, rX: 180));
                                pos.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: z, rY: 90));
                            }

                            for (int i = 0; i < FanCount - 1; i++)
                            {
                                double z = zTranslation;
                                z -= Length / FanCount;

                                pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: -z, rY: -90));
                                pos.Add(PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: -z, rY: -90, rX: 180));
                            }
                        }
                    }


                    // End panel brace stiffeners
                    if (BraceType.Contains("L"))
                    {
                        double bentClipWidth = KneeClipBent.GetWidth(out _, out double holeToPlateCenter);
                        double xTranslation = -Width / 2 + ColumnCenterToPlenumEndClipHole() + holeToPlateCenter + bentClipWidth / 2 + clearance;
                        double zTranslation = Length / 2 + Beam_Depth / 2;

                        var endBraceStiffeners = new List<PositionData>
                        {
                            PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, rZ: 180),
                            PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: zTranslation),

                            PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: -zTranslation, rY: 180),
                            PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: -zTranslation, rY: 180, rZ: 180),
                        };
                       pos.AddRange(endBraceStiffeners);
                    }
                }

                return pos;
            }
        }
    }
}
