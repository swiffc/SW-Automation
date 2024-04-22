using ModelTools;
using SolidWorks.Interop.sldworks;
using System.Collections.Generic;
using static FileTools.FileTools;
using static Plenum.Plenum;
using aTools = ModelTools.AssemblyTools;
using cTools = ModelTools.ReleaseCOM;
using mTools = Tools.ModelTools;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Plenum
{
    internal class DividerAngle : Part
    {
        // Static properties
        public static bool Enabled
        {
            get
            {
                return MidColumns && FanCount > 1 ? true : false;
            }
        }
        internal static double THK => DividerPanel.THK;
        internal static double Gauge => 1.75;
        internal static double ShortGauge => 1.125;
        internal static double LongLeg => 3;
        internal static double ShortLeg => 2;


        // Constructor
        public DividerAngle(Design callerType) : base(callerType) { }

        public override RawMaterial Shape => RawMaterial.Plate;
        public override string Size => THK.ToString();
        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            mTools.EditDimension("Length", "L", CornerAngle.LocalLength, modelDoc2);
            mTools.EditDimension("Hole0", "sk:Hole0", CornerAngle.HolePositions[0], modelDoc2);
            mTools.EditDimension("Hole1", "sk:Hole0", CornerAngle.HolePositions[1], modelDoc2);
            mTools.EditDimension("Hole2", "sk:Hole0", CornerAngle.HolePositions[2], modelDoc2);
            mTools.EditDimension("Hole3", "sk:Hole0", CornerAngle.HolePositions[3], modelDoc2);
            mTools.EditDimension("Hole4", "sk:Hole0", CornerAngle.HolePositions[4], modelDoc2);
            mTools.EditDimension("Hole5", "sk:Hole0", CornerAngle.HolePositions[5], modelDoc2);
            mTools.EditDimension("Hole6", "sk:Hole6", CornerAngle.HolePositions[6], modelDoc2);
            mTools.EditDimension("Hole7", "sk:Hole6", CornerAngle.HolePositions[7], modelDoc2);
            mTools.EditDimension("Hole8", "sk:Hole6", CornerAngle.HolePositions[8], modelDoc2);
            mTools.EditDimension("Hole9", "sk:Hole6", CornerAngle.HolePositions[9], modelDoc2);
            mTools.EditDimension("Hole10", "sk:Hole6", CornerAngle.HolePositions[10], modelDoc2);
        }


        // Property overrides
        public override List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {
                    double xTranslation = Width / 2 - Beam_WebTHK / 2;
                    double yTranslation = CornerAngle.YTranslation;
                    double zTranslation = Length / 2 - EndPanel_THK / 2;
                    double zTranslation2 = Length / 2 + EndPanel_THK / 2;
                    if (CallerType == Design.Johnson || CallerType == Design.Legacy)
                    {
                        xTranslation = Width / 2 - Beam_Depth / 2;
                    }

                    _position = new List<PositionData>();

                    for (int i = 1; i < FanCount; i++)
                    {
                        double zOffset = i * (Length / FanCount);
                        _position.Add(PositionData.Create(tX: xTranslation, tY: -yTranslation, tZ: zTranslation - zOffset));
                        _position.Add(PositionData.Create(tX: -xTranslation, tY: -yTranslation, tZ: zTranslation2 - zOffset, rY: 180));
                    }
                }
                return _position;
            }
        }
        public override string StaticPartNo => "185";
        protected override AssemblyDoc ParentAssembly => Plenum.AssemblyDoc;



    }
}
