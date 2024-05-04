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
                    if (FanDiameter > 144)
                        return 32;
                    else if (FanDiameter > 96)
                        return 24;
                    else if (FanDiameter > 72)
                        return 16;
                    else
                        return 12;
                }
            }
            public int ShaftUp
            {
                get
                {
                    if (_fanDiameterFeet > 14)
                        return 10;
                    if (_fanDiameterFeet > 13)
                        return 9;
                    if (_fanDiameterFeet > 11)
                        return 8;
                    if (_fanDiameterFeet > 9)
                        return 7;
                    if (_fanDiameterFeet > 7)
                        return 6;
                    if (_fanDiameterFeet > 5)
                        return 5;
                    else
                        return 4;
                }
            }
        }
        public override RawMaterial Shape => RawMaterial.Plate;
        public override string Size => "0.1344";

        // Constructor
        public FanRing(Design callerType) : base(callerType) { }


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            double angle1 = FindGapAngle(mTools.InterferenceClearance / 2, Radius);
            double angle2 = SectionCount == 2 ? angle1 : angle1 + 90;

            mTools.EditDimension("Angle1", "sk:Web", angle1, modelDoc2);
            mTools.EditDimension("Angle2", "sk:Web", angle2, modelDoc2);
            mTools.EditDimension("Diameter", "sk:Web", Radius * 2, modelDoc2);
            mTools.EditDimension("Depth", "Web", Default.FanRing_Depth, modelDoc2);

            if (MotorShaft_Orientation.ToLower().Contains("down"))
            {
                mTools.EditDimension("Count", "sk:ShaftDownRadialSlot", FanRing.RadialCount.ShaftDown, modelDoc2);
                mTools.EditDimension("Count", "ShaftDownRadialSlots", FanRing.RadialCount.ShaftDown, modelDoc2);
            }
            else
            {
                mTools.EditDimension("Count", "ShaftUpRadialSlots", FanRing.RadialCount.ShaftUp, modelDoc2);
            }

            mTools.HolePattern(Default.FanRing_Depth - 4, out double count, out double spacing, 4);
            mTools.EditDimension("Count", "sk:Cuts", count, modelDoc2);
            mTools.EditDimension("Spacing", "sk:Cuts", spacing, modelDoc2);
        }
        protected override void FeatureSuppression(ModelDoc2 modelDoc2)
        {
            if (MotorShaft_Orientation.ToLower().Contains("down"))
            {
                mTools.SuppressFeatures(false, modelDoc2, "ShaftDownRadialSlot");
                mTools.SuppressFeatures(false, modelDoc2, "ShaftDownRadialSlots");
                mTools.SuppressFeatures(true, modelDoc2, "ShaftUpRadialSlot");
                mTools.SuppressFeatures(true, modelDoc2, "ShaftUpRadialSlots");
            }
            else
            {
                mTools.SuppressFeatures(true, modelDoc2, "ShaftDownRadialSlot");
                mTools.SuppressFeatures(true, modelDoc2, "ShaftDownRadialSlots");
                mTools.SuppressFeatures(false, modelDoc2, "ShaftUpRadialSlot");
                mTools.SuppressFeatures(false, modelDoc2, "ShaftUpRadialSlots");
            }
        }


        // Private methods
        private double FindGapAngle(double gapFromCenter, double ringRadius)
        {
            double cosine = gapFromCenter / ringRadius;
            double angleRadians = Math.Acos(cosine);
            double angleDegrees = angleRadians * (180.0 / Math.PI);
            return 90 - angleDegrees;
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
        private int SectionCount => FanDiameter <= 60 ? 2 : 4;
    }
}
