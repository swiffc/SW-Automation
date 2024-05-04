using ModelTools;
using Plenum.Helpers.Static;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using static FileTools.FileTools;
using static Plenum.Plenum;
using aTools = ModelTools.AssemblyTools;
using bTools = ModelTools.BendTable;
using cTools = ModelTools.ReleaseCOM;
using mTools = Tools.ModelTools;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;
using static FileTools.Properties.Settings;

namespace Plenum
{
    internal class CornerAngle : Part
    {
        // Static properties
        public static bool Enabled { get; set; } = true;
        public static List<double> HolePositions
        {
            get
            {
                double negateHole =  + 1;

                double holeToEdge = 1.25;
                double lastHole = LocalLength - holeToEdge;
                double spaceAvailable = LocalLength - 2 * holeToEdge;
                double segmentLength = spaceAvailable / MinHoleCount;

                double hole0 = holeToEdge;
                double hole1 = holeToEdge + segmentLength;
                double hole2 = Plenum_Depth <= 18 ? lastHole : hole1 + segmentLength;
                double hole3 = Plenum_Depth <= 18 ? negateHole : Plenum_Depth <= 24 ? lastHole : hole2 + segmentLength;
                double hole4 = Plenum_Depth <= 18 ? negateHole : Plenum_Depth <= 24 ? negateHole : Plenum_Depth <= 30 ? lastHole : hole3 + segmentLength;
                double hole5 = Plenum_Depth <= 18 ? negateHole : Plenum_Depth <= 24 ? negateHole : Plenum_Depth <= 30 ? negateHole : lastHole;


                double holeToEdge2 = 3.25;
                double lastHole2 = LocalLength - holeToEdge2;
                double spaceAvailable2 = LocalLength - 2 * holeToEdge2;
                double segmentLength2 = spaceAvailable2 / (MinHoleCount - 1);

                double hole6 = holeToEdge2;
                double hole7 = Plenum_Depth <= 18 ? lastHole2 : holeToEdge2 + segmentLength2;
                double hole8 = Plenum_Depth <= 18 ? negateHole : Plenum_Depth <= 24 ? lastHole2 : hole7 + segmentLength2;
                double hole9 = Plenum_Depth <= 18 ? negateHole : Plenum_Depth <= 24 ? negateHole : Plenum_Depth <= 30 ? lastHole2 : hole8 + segmentLength2;
                double hole10 = Plenum_Depth <= 18 ? negateHole : Plenum_Depth <= 24 ? negateHole : Plenum_Depth <= 30 ? negateHole : lastHole2;

                return new List<double>
                {
                    hole0,
                    hole1,
                    hole2,
                    hole3,
                    hole4,
                    hole5,

                    hole6,
                    hole7,
                    hole8,
                    hole9,
                    hole10
                };
            }
        }
        internal static double LocalLength
        {
            get
            {
                return Plenum_Depth - YTranslation * 2;
            }
        }
        internal static double YTranslation
        {
            get
            {
                double maxTHK = Math.Max(EndPanel_THK, SidePanel_THK);
                return maxTHK + bTools.GetBendRadius(maxTHK);
            }
        }
        internal static int MinHoleCount
        {
            get
            {
                if (Plenum_Depth <= 18)
                {
                    return 2;
                }
                else if (Plenum_Depth <= 24)
                {
                    return 3;
                }
                else if (Plenum_Depth <= 30)
                {
                    return 4;
                }
                else
                {
                    return 5;
                }
            }
        }
        internal static double Gauge => 1.75;
        internal static double Leg => 3;



        // Constructor
        public CornerAngle(Design callerType) : base(callerType) { }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            mTools.EditDimension("Length", "L", LocalLength, modelDoc2);
            mTools.EditDimension("Hole0", "sk:Hole0", HolePositions[0], modelDoc2);
            mTools.EditDimension("Hole1", "sk:Hole0", HolePositions[1], modelDoc2);
            mTools.EditDimension("Hole2", "sk:Hole0", HolePositions[2], modelDoc2);
            mTools.EditDimension("Hole3", "sk:Hole0", HolePositions[3], modelDoc2);
            mTools.EditDimension("Hole4", "sk:Hole0", HolePositions[4], modelDoc2);
            mTools.EditDimension("Hole5", "sk:Hole0", HolePositions[5], modelDoc2);
            mTools.EditDimension("Hole6", "sk:Hole6", HolePositions[6], modelDoc2);
            mTools.EditDimension("Hole7", "sk:Hole6", HolePositions[7], modelDoc2);
            mTools.EditDimension("Hole8", "sk:Hole6", HolePositions[8], modelDoc2);
            mTools.EditDimension("Hole9", "sk:Hole6", HolePositions[9], modelDoc2);
            mTools.EditDimension("Hole10", "sk:Hole6", HolePositions[10], modelDoc2);

            mTools.EditDimension("Gage0", "sk:Hole0", PlenumDesign == Design.Standard ? Gauge - SidePanel_THK : Gauge, modelDoc2);
            mTools.EditDimension("Gage6", "sk:Hole6", PlenumDesign == Design.Legacy ? Gauge - EndPanel_THK : Gauge, modelDoc2);
        }


        // Property overrides
        public override List<PositionData> Position
        {
            get
            {

                double yTranslation = YTranslation;
                double xTranslation, zTranslation;
                double zTranslation2;

                switch (CallerType)
                {
                    case Design.Standard:
                        xTranslation = Plenum_Width / 2 - SidePanel_THK;
                        zTranslation = zTranslation2 = Plenum_Length / 2 - Beam_Depth / 2;
                        break;
                    case Design.Johnson:
                        xTranslation = Plenum_Width / 2 + Beam_Depth / 2;
                        zTranslation = Plenum_Length / 2 + Default.Johnson_ExtraLength;
                        zTranslation2 = -(Plenum_Length / 2);
                        break;
                    case Design.Legacy:
                        xTranslation = Plenum_Width / 2 - Beam_Depth / 2;
                        zTranslation = zTranslation2 = Plenum_Length / 2 - EndPanel_THK;
                        break;
                    default:
                        xTranslation = 0;
                        zTranslation = 0;
                        zTranslation2 = 0;
                        break;
                }

                // Corners
                _position = new List<PositionData>
                    {
                        PositionData.Create(tX: xTranslation, tY: -yTranslation, tZ: zTranslation),
                        PositionData.Create(tX: -xTranslation, tY: -yTranslation-LocalLength, tZ: zTranslation, rZ: 180),
                        PositionData.Create(tX: xTranslation, tY: -yTranslation-LocalLength, tZ: -zTranslation, rX: 180),
                        PositionData.Create(tX: -xTranslation, tY: -yTranslation, tZ: -zTranslation, rY: 180)
                    };

                if (!Mid_Columns)
                    xTranslation = Plenum_Width / 2 + Beam_Depth / 2 - Beam_FlangeTHK - SidePanel_THK;

                if (Fan_Count > 1 && Mid_Columns)
                {
                    for (int i = 1; i < Fan_Count; i++)
                    {
                        double zOffset = i * (Plenum_Length / Fan_Count);
                        if (CallerType == Design.Standard)
                        {
                            _position.Add(PositionData.Create(tX: xTranslation, tY: -yTranslation, tZ: zTranslation2 - zOffset));
                            _position.Add(PositionData.Create(tX: -xTranslation, tY: -yTranslation - LocalLength, tZ: zTranslation2 - zOffset, rZ: 180));
                            _position.Add(PositionData.Create(tX: xTranslation, tY: -yTranslation - LocalLength, tZ: -(zTranslation2 - zOffset), rX: 180));
                            _position.Add(PositionData.Create(tX: -xTranslation, tY: -yTranslation, tZ: -(zTranslation2 - zOffset), rY: 180));
                        }

                    }
                }

                if (!Mid_Columns)
                {
                    double zTranslation3 = Plenum_Length / 2 + DividerPanel.THK / 2;
                    double xTranslation3;
                    if (CallerType == Design.Standard)
                    {
                        xTranslation3 = Plenum_Width / 2;
                    }
                    else if (CallerType == Design.Legacy)
                    {
                        xTranslation3 = Plenum_Width / 2 + Beam_Depth/2 - Beam_FlangeTHK - SidePanel_THK;
                    }
                    else if (CallerType == Design.Johnson)
                    {
                        xTranslation3 = Plenum_Width / 2 + Beam_Depth / 2;
                    }
                    else
                    {
                        xTranslation3 = Plenum_Width / 2; // temporary logic
                    }

                    for (int i = 1; i < Fan_Count; i++)
                    {
                        zTranslation3 -= Plenum_Length / Fan_Count;
                        _position.Add(PositionData.Create(tX: xTranslation3, tY: -yTranslation, tZ: zTranslation3 - DividerPanel.THK));
                        _position.Add(PositionData.Create(tX: -xTranslation3, tY: -yTranslation, tZ: zTranslation3, rY: 180));
                    }
                }

                return _position;
            }
        }
        public override string StaticPartNo => "186";
        protected override AssemblyDoc ParentAssembly => Plenum.AssemblyDoc;
        public override RawMaterial Shape => RawMaterial.Angle;
        public override string Size => "L3x3x0.1875";


    }
}
