using ModelTools;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.FileTools;
using static Plenum.Plenum;
using aTools = ModelTools.AssemblyTools;
using cTools = ModelTools.ReleaseCOM;
using mTools = Tools.ModelTools;

namespace Plenum
{
    internal abstract class PlanBrace : Part
    {
        // Static properties
        public static bool Enabled
        {
            get
            {
                bool value = MotorShaft.ToLower() == "up" ? true : false;
                return value;
            }
        }
        public override RawMaterial Shape => RawMaterial.Angle;
        public override string Size => "L2.5x2.5x0.1875";

        // Constructor
        public PlanBrace(CallerType callerType) : base(callerType) { }


        // Protected methods
        protected double GetNominalLength()
        {
            double slotGauge = 1.5;
            double sectionLength = Length / FanCount + (CallerType == CallerType.Johnson ? Johnson.ExtraLength : 0);
            double sectionThird = sectionLength / 3 - slotGauge * 2;
            double angle = 45;

            mTools.AAS(angle, out _, sectionThird, out double hypotenuse);
            double result = slotGauge / Math.Cos(angle * Math.PI / 180);
            return hypotenuse + result;
        }
        protected void GetPositionAtNominalLength(out double xTranslation, out double yTranslation, out double angle, bool endBrace = false)
        {
            switch (CallerType)
            {
                case CallerType.Standard:
                    xTranslation = Width / 2;
                    break;
                case CallerType.Johnson:
                    xTranslation = Width / 2 + Beam.Depth / 2;
                    break;
                case CallerType.Legacy:
                    xTranslation = Width / 2 + Beam.Depth / 2 - Beam.FlangeTHK - SidePanel.THK;
                    break;
                default: throw new ArgumentException();
            }

            yTranslation = -4;
            angle = 45;

            double sectionLength = Length / FanCount + (CallerType == CallerType.Johnson ? Johnson.ExtraLength : 0);
            double sectionThird = sectionLength / 3;

            mTools.AAS(angle, out double oppositeSide, sectionThird / 2, out _);
            xTranslation -= oppositeSide;
        }
        protected void AdjustLengthZ(double adjustmentValue, ref double length)
        {
            double angle = 45;
            double adjustment = adjustmentValue / Math.Cos(angle * Math.PI / 180);
            length += adjustment;
        }
        protected void AdjustPositionZ(double adjustmentValue, ref double xTranslation, bool endBrace = false)
        {
            xTranslation += adjustmentValue * (endBrace ? -1 : 1);
        }


        // Property overrides
        protected override AssemblyDoc ParentAssembly => Plenum.AssemblyDoc;
    }
}
