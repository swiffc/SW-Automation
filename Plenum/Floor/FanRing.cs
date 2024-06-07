using ModelTools;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using aTools = ModelTools.AssemblyTools;
using cTools = ModelTools.ReleaseCOM;
using mTools = Tools.ModelTools;
using bTools = ModelTools.BendTable;
using static Plenum.Plenum;
using Plenum.Helpers.Static;
using static FileTools.FileTools;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;
using static FileTools.Properties.Settings;
using static FileTools.StaticFileTools;

namespace Plenum.Floor
{
    internal class FanRing : Part
    {
        // Static properties
        public static bool Enabled { get; set; } = true;
        public static double Radius => (FloorPanel.Diameter - mTools.InterferenceClearance) / 2;
        public static RadialHoleCountExtension RadialCount { get; set; } = new RadialHoleCountExtension();
        public class RadialHoleCountExtension
        {
            public int ShaftDown
            {
                get
                {
                    if (FanDiameter_Inches > 144)
                        return 32;
                    else if (FanDiameter_Inches > 96)
                        return 24;
                    else if (FanDiameter_Inches > 72)
                        return 16;
                    else
                        return 12;
                }
            }
            public int ShaftUp
            {
                get
                {
                    if (FanDiameter_Feet > 14)
                        return 10;
                    if (FanDiameter_Feet > 13)
                        return 9;
                    if (FanDiameter_Feet > 11)
                        return 8;
                    if (FanDiameter_Feet > 9)
                        return 7;
                    if (FanDiameter_Feet > 7)
                        return 6;
                    if (FanDiameter_Feet > 5)
                        return 5;
                    else
                        return 4;
                }
            }
        }
        public override RawMaterial Shape => RawMaterial.Plate;
        public override string Size => "0.1344";
        static public bool IsRibbed => Default.FanRing_Depth > 18 ? true : false;


        // Constructor
        public FanRing(Design callerType) : base(callerType) { }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            double angle1 = FindGapAngle(mTools.InterferenceClearance / 2, Radius);
            double angle2 = SectionCount == 2 ? angle1 : angle1 + 90;

            // Ring
            mTools.EditDimension("Depth", "sk:Profile", Default.FanRing_Depth, modelDoc2);
            mTools.EditDimension("Rib", "sk:Profile", IsRibbed ? 1 : 2, modelDoc2);
            mTools.EditDimension("Angle1", "sk:Path", angle1, modelDoc2);
            mTools.EditDimension("Angle2", "sk:Path", angle2, modelDoc2);
            mTools.EditDimension("Diameter", "sk:Path", Radius * 2, modelDoc2);


            // Web
            double topToFirstSquare = 2;
            double bottomtoLastSquare = MotorShaftUp ? 2.75 : 2;
            double span = Ring_Depth - topToFirstSquare - bottomtoLastSquare;
            mTools.HolePattern(span, out double count, out double spacing, 5);
            mTools.EditDimension("Count", "sk:RolledSquarePunch", IsRibbed ? 2 : count, modelDoc2);
            mTools.EditDimension("Spacing", "sk:RolledSquarePunch", IsRibbed ? span : spacing, modelDoc2);


            // Flange
            int radialCount;
            double angle11;
            double angle22;
            double slotAngle;
            double arc;
            if (MotorShaftDown)
            {// shaft down
                radialCount = RadialCount.ShaftDown;
                angle11 = 0.001;
                slotAngle = 180 / radialCount;
                angle22 = 360 - slotAngle;
                arc = 0.001;
            }
            else
            {// shaft up
                radialCount = RadialCount.ShaftUp;
                angle11 = angle1;
                angle22 = SectionCount == 2 ? 180 - angle1 : 180 - angle2;
                slotAngle = ShaftUp_SlotAngle();
                arc = 3;
            }
            mTools.EditDimension("Count", "Slots", radialCount, modelDoc2);
            mTools.EditDimension("Angle11", "sk:Slot", angle11, modelDoc2);
            mTools.EditDimension("Angle22", "sk:Slot", angle22, modelDoc2);
            mTools.EditDimension("SlotAngle", "sk:Slot", slotAngle, modelDoc2);
            mTools.EditDimension("Arc", "sk:Slot", arc, modelDoc2);


            // Flat 
            mTools.EditDimension("Count", "sk:SpliceCarriageSlot", IsRibbed ? 2 : count, modelDoc2);
            mTools.EditDimension("Spacing", "sk:SpliceCarriageSlot", IsRibbed ? span + 1.37609108 : spacing, modelDoc2);
            mTools.EditDimension("RibOffset", "sk:SpliceCarriageSlot", IsRibbed ? 0.56397915 : Ring_Depth, modelDoc2);
            mTools.EditDimension("Count", "sk:Perforation", IsRibbed ? 2 : count, modelDoc2);
            mTools.EditDimension("Spacing", "sk:Perforation", IsRibbed ? span + 1.37609108 : spacing, modelDoc2);
            mTools.EditDimension("SlotOffset", "sk:SpliceStrutSlots", IsRibbed ? 1.37609108 : 0.001, modelDoc2);
        }
        protected override void FeatureSuppression(ModelDoc2 modelDoc2)
        {
            if (IsRibbed)
                mTools.SuppressFeatures_Legacy(false, modelDoc2,
                    "RibSquarePunch",
                    "RibSquarePunchMirrorHorz",
                    "RibSquarePunchMirrorVertical");
            else // straight
                mTools.SuppressFeatures_Legacy(true, modelDoc2,
                    "RibSquarePunch",
                    "RibSquarePunchMirrorHorz",
                    "RibSquarePunchMirrorVertical");

            string[] configsToExclude = { StaticPartNo };
            mTools.SuppressFeatures_Legacy(false, configsToExclude, modelDoc2,  "FlatPattern");
            if (MotorShaftDown)
                mTools.SuppressFeatures_Legacy(true, modelDoc2, // shaft down
                    "StrutSlot",
                    "StrutSlotMirror",
                    "SpliceStrutSlots");
            else
                mTools.SuppressFeatures_Legacy(false, modelDoc2, // shaft up
                    "StrutSlot",
                    "StrutSlotMirror",
                    "SpliceStrutSlots");
            mTools.SuppressFeatures_Legacy(true, configsToExclude, modelDoc2,  "FlatPattern");
        }


        // Private methods
        private static double FindGapAngle(double gapFromCenter, double ringRadius)
        {
            double cosine = gapFromCenter / ringRadius;
            double angleRadians = Math.Acos(cosine);
            double angleDegrees = angleRadians * (180.0 / Math.PI);
            return 90 - angleDegrees;
        }
        private static double ShaftUp_SlotAngle()
        {
            double arcLength = 3.0 + 0.125;
            double boltCircleRadius = Radius + 1.125;

            double angleInRadians = arcLength / boltCircleRadius;
            double angleInDegrees = angleInRadians * (180.0 / Math.PI);

            return angleInDegrees;
        }


        // Property overrides
        public override List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {
                    _position = new List<PositionData>();

                    var zTranslation = FanCenter.ZTranslation(CallerType);
                    double yTranslation = Plenum_Depth - Math.Max(EndPanel_THK, SidePanel_THK);

                    for (int i = 0; i < zTranslation.Count; i++)
                    {
                        _position.Add(PositionData.Create(tZ: zTranslation[i], tY: -yTranslation));
                        _position.Add(PositionData.Create(tZ: zTranslation[i], tY: -yTranslation, rY: 180));

                        if (SectionCount == 4)
                        {
                            _position.Add(PositionData.Create(tZ: zTranslation[i], tY: -yTranslation, rY: 90));
                            _position.Add(PositionData.Create(tZ: zTranslation[i], tY: -yTranslation, rY: 270));
                        }
                    }
                }
                return _position;
            }
        }
        public override string StaticPartNo => "196";
        protected override AssemblyDoc ParentAssembly => Plenum.AssemblyDoc;


        // Private properties
        private static int SectionCount => FanDiameter_Inches <= 60 ? 2 : 4;
    }
}
