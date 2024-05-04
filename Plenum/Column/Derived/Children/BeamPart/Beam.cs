using ModelTools;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using static FileTools.FileTools;
using static Plenum.Plenum;
using aTools = ModelTools.AssemblyTools;
using cTools = ModelTools.ReleaseCOM;
using mTools = Tools.ModelTools;
using static FileTools.CommonData.CommonData;
using static FileTools.Properties.Settings;

namespace Plenum
{

    internal abstract class Beam : Part
    {
        // Constructor
        protected Beam(Design callerType) : base(callerType) { }


        // Private methods
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            mTools.EditDimension("Depth", "sk:Beam", Beam_Depth, modelDoc2);
            mTools.EditDimension("WebTHK", "sk:Beam", Beam_WebTHK, modelDoc2);
            mTools.EditDimension("FlangeWidth", "sk:Beam", Beam_FlangeWidth, modelDoc2);
            mTools.EditDimension("FlangeTHK", "sk:Beam", Beam_FlangeTHK, modelDoc2);
            mTools.EditDimension("K", "sk:Beam", Beam_K, modelDoc2);
            mTools.EditDimension("K1", "sk:Beam", Beam_K1, modelDoc2);
            mTools.EditDimension("Length", "Beam", LocalLength, modelDoc2);

            double johnsonPlate = CallerType == Design.Johnson ? 0.5 : 0;
            if (CallerType == Design.Legacy || CallerType == Design.Johnson)
            {
                mTools.EditDimension("Hole0", "sk:FlangeHole", CornerAngle.HolePositions[6] + CornerAngle.YTranslation - johnsonPlate, modelDoc2);
                mTools.EditDimension("Hole1", "sk:FlangeHole", CornerAngle.HolePositions[7] + CornerAngle.YTranslation - johnsonPlate, modelDoc2);
                mTools.EditDimension("Hole2", "sk:FlangeHole", CornerAngle.HolePositions[8] + CornerAngle.YTranslation - johnsonPlate, modelDoc2);
                mTools.EditDimension("Hole3", "sk:FlangeHole", CornerAngle.HolePositions[9] + CornerAngle.YTranslation - johnsonPlate, modelDoc2);
                mTools.EditDimension("Hole4", "sk:FlangeHole", CornerAngle.HolePositions[10] + CornerAngle.YTranslation - johnsonPlate, modelDoc2);
                mTools.EditDimension("Hole5", "sk:FlangeHole", PlenumColumn_Height + 1, modelDoc2);

                mTools.EditDimension("Gage", "sk:FlangeHole2", DividerAngle.ShortGauge + EndPanel_THK / 2, modelDoc2);

            }
            else
            {
                mTools.EditDimension("Hole0", "sk:FlangeHole", CornerAngle.HolePositions[0] + CornerAngle.YTranslation, modelDoc2);
                mTools.EditDimension("Hole1", "sk:FlangeHole", CornerAngle.HolePositions[1] + CornerAngle.YTranslation, modelDoc2);
                mTools.EditDimension("Hole2", "sk:FlangeHole", CornerAngle.HolePositions[2] + CornerAngle.YTranslation, modelDoc2);
                mTools.EditDimension("Hole3", "sk:FlangeHole", CornerAngle.HolePositions[3] + CornerAngle.YTranslation, modelDoc2);
                mTools.EditDimension("Hole4", "sk:FlangeHole", CornerAngle.HolePositions[4] + CornerAngle.YTranslation, modelDoc2);
                mTools.EditDimension("Hole5", "sk:FlangeHole", CornerAngle.HolePositions[5] + CornerAngle.YTranslation, modelDoc2);

                mTools.EditDimension("Gage", "sk:FlangeHole2", CornerAngle.Gauge, modelDoc2);
                AdjustWebHoles(modelDoc2);
            }
        }

        protected virtual void AdjustWebHoles(ModelDoc2 modelDoc2) { }
        public static void ResetSize()
        {
            _depth = null;
            _webTHK = null;
            _flangeWidth = null;
            _flangeTHK = null;
            _k = null;
            _k1 = null;
            _flangeGage = null;
            _webGage = null;
        }


        public override List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {
                    double yTranslation = 0;
                    if (CallerType == Design.Johnson)
                    {
                        yTranslation = -0.5;
                    }
                    _position = new List<PositionData>
                    {
                        PositionData.Create(tY: yTranslation)
                    };

                }
                return _position;
            }
        }
        public override RawMaterial Shape => RawMaterial.Beam;
        public override string Size => PlenumColumn.Size;

        // Internal properties
        internal double LocalLength
        {
            get
            {
                if (CallerType == Design.Johnson)
                {
                    return PlenumColumn_Height - CapPlate.THK - 0.5;
                }
                else
                {
                    return PlenumColumn_Height - CapPlate.THK;
                }
            }
        }


        // Private properties
        private static double? _depth;
        private static double? _webTHK;
        private static double? _flangeWidth;
        private static double? _flangeTHK;
        private static double? _k;
        private static double? _k1;
        private static double? _flangeGage;
        private static double? _webGage;

    }
}
