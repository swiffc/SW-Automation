using ModelTools;
using Plenum.Helpers.Static;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Documents;
using static FileTools.FileTools;
using static Plenum.Plenum;
using cTools = ModelTools.ReleaseCOM;
using mTools = Tools.ModelTools;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;
using static FileTools.Properties.Settings;

namespace Plenum.Floor
{
    internal class FloorSplice : Part
    {
        public static bool Enabled { get; set; } = FloorPanel.SpliceRequired;
        public static double LengthOverride => Default.FloorSplice_LengthOverride;

        // Constructor
        public FloorSplice(Design callerType) : base(callerType) { }


        // Static Properties
        internal static double NominalLength
        {
            get
            {
                if (LengthOverride > 0 )
                    return LengthOverride;
                else return FanDiameter_Inches <= 156 ? 30 : 72;
            }
        }


        // Property overrides
        public override string StaticPartNo => "192";
        protected override AssemblyDoc ParentAssembly => Plenum.AssemblyDoc;
        public override List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {
                    _position = new List<PositionData>();

                    if (FloorPanel.SpliceRequired)
                    {
                        var zTranslation = FanCenter.ZTranslation(CallerType);
                        double yTranslation = Plenum_Depth - Math.Max(EndPanel_THK, SidePanel_THK);

                        for (int i = 0; i < Fan_Count; i++)
                        {
                            _position.Add(PositionData.Create(tZ: zTranslation[i], tY: -yTranslation));
                            _position.Add(PositionData.Create(tZ: zTranslation[i], tY: -yTranslation, rY: 180));
                        }
                    }
                }

                return _position;
            }
        }
        public override RawMaterial Shape => RawMaterial.Plate;
        public override string Size => FloorPanel.THK.ToString();


        // Method overrides
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            mTools.EditDimension("Width", "sk:Plate", FloorPanel.GetWidth(CallerType) + mTools.AssemblyClearance / 2, modelDoc2);
            mTools.EditDimension("Length", "sk:Plate", NominalLength - mTools.AssemblyClearance, modelDoc2);
            mTools.EditDimension("Diameter", "sk:Plate", FloorPanel.Diameter, modelDoc2);

            mTools.EditDimension("SideGauge", "sk:FloorHole", SidePanel.Gauge - SidePanel_THK - SidePanel.R, modelDoc2);

            mTools.HolePattern(NominalLength - mTools.AssemblyClearance - 3 * 2, out double count, out double spacing);
            mTools.EditDimension("Spacing", "sk:FloorHole", spacing, modelDoc2);
            mTools.EditDimension("Count", "sk:FloorHole", count, modelDoc2);

            if (MotorShaftDown)
            {
                mTools.EditDimension("BoltCircleR", "sk:ShaftDownRadialHole", FanRing.Radius + 1.125, modelDoc2);
                mTools.EditDimension("Count", "sk:ShaftDownRadialHole", FanRing.RadialCount.ShaftDown, modelDoc2);
            }
            else
            {
                mTools.EditDimension("BoltCircleR", "sk:ShaftUpRadialHoles", FanDiameter_Inches / 2 + 0.375 + 1.125, modelDoc2);
                mTools.EditDimension("Count", "sk:ShaftUpRadialHoles", FanRing.RadialCount.ShaftUp, modelDoc2);

            }

            double flange192Span = FloorPanel.GetWidth(CallerType) - FloorPanel.Diameter / 2 - 1.5 * 2 + 0.0625
                + (FloorPanel.SpliceRequired == true ? 1.7283 : 0); // Measurement from Solidworks for 30" long 192splice
            mTools.HolePattern(flange192Span, out double count192, out double spacing192);
            mTools.EditDimension("Count", "sk:FlangeHole", count192, modelDoc2);
            mTools.EditDimension("Spacing", "sk:FlangeHole", spacing192, modelDoc2);
        }
        protected override void FeatureSuppression(ModelDoc2 modelDoc2)
        {
            if (MotorShaftDown)
            {
                mTools.SuppressFeatures(false, modelDoc2, "ShaftDownRadialHole");
                mTools.SuppressFeatures(false, modelDoc2, "ShaftDownRadialHoles");
                mTools.SuppressFeatures(true, modelDoc2, "ShaftUpRadialHoles");
            }
            else
            {
                mTools.SuppressFeatures(true, modelDoc2, "ShaftDownRadialHole");
                mTools.SuppressFeatures(true, modelDoc2, "ShaftDownRadialHoles");
                mTools.SuppressFeatures(false, modelDoc2, "ShaftUpRadialHoles");
            }
        }
    }
}
