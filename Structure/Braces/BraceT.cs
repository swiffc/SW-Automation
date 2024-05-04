using FileTools.Base;
using ModelTools;
using Structure.Columns.Derived.Children;
using Structure.Columns.Derived.Children.Derived;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Tools.ModelTools;
using static Plenum.Plenum;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace Structure.Braces
{
    internal class BraceT : Part
    {
        // Constructor
        public BraceT(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            CalculateLengthAndPositionData(out double length, out _, out _);
            EditDimension("Length", "T", length);
            EditDimension("Diameter", "sk:Hole", HoleDiameter_Structural);

            EditDimension("Depth", "sk:T", WT_Depth);
            EditDimension("StemTHK", "sk:T", WT_StemTHK);
            EditDimension("FlangeWidth", "sk:T", WT_FlangeWidth);
            EditDimension("FlangeTHK", "sk:T", WT_FlangeTHK);
            EditDimension("K", "sk:T", WT_K);
            EditDimension("K1", "sk:T", WT_K1);
            EditDimension("FlangeGage", "sk:Hole", WT_FlangeGage);
        }


        private void AddOtherSideBrace(ref List<PositionData> list, PositionData position)
        {
            var otherSideBrace = position;
            otherSideBrace.TranslationX *= -1;
            otherSideBrace.RotationX *= -1;
            otherSideBrace.RotationY = 180;
            list.Add(otherSideBrace);
        }
        private void AddOppositeSideBrace(ref List<PositionData> list, PositionData position)
        {
            var oppositeSideBrace = position;
            oppositeSideBrace.TranslationZ *= -1;
            oppositeSideBrace.RotationX *= -1;
            list.Add(oppositeSideBrace);
        }
        private void AddOtherOppositeSideBrace(ref List<PositionData> list, PositionData position)
        {
            var otherOppositeSideBrace = position;
            otherOppositeSideBrace.TranslationX *= -1;
            otherOppositeSideBrace.TranslationZ *= -1;
            otherOppositeSideBrace.RotationY = 180;
            list.Add(otherOppositeSideBrace);
        }


        // Property overrides
        public override bool Enabled => new[] { "T", "TX" }.Contains(BraceType);
        public override string StaticPartNo => "131T";
        public override Shape RawMaterialShape => Shape.Tee;
        public override string SizeOrThickness => "WT";
        public override List<PositionData> Position
        {
            get
            {
                CalculateLengthAndPositionData(out double length, out _, out PositionData position);

                var pos = new List<PositionData>
                {
                    position
                };

                AddOtherSideBrace(ref pos, position);
                AddOppositeSideBrace(ref pos, position);
                AddOtherOppositeSideBrace(ref pos, position);

                return pos;
            }
        }
    }
}
