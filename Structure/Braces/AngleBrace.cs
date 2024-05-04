using FileTools.Base;
using Structure.Braces.Derived;
using Structure.Columns.Derived.Children;
using static FileTools.FileTools;
using static FileTools.CommonData.CommonData;
using static FileTools.Properties.Settings;

namespace Structure.Braces
{
    internal abstract class AngleBrace : Part
    {
        // Constructor
        protected AngleBrace(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "L", (BraceType == "L" || BraceType == "LL") ? BraceL.LocalLength : BraceX.LocalLength);
            EditDimension("Diameter", "sk:Hole", HoleDiameter_Structural);
            EditDimension("Leg1", "sk:L", Default.AngleBrace_Leg1);
            EditDimension("Leg2", "sk:L", Default.AngleBrace_Leg2);
            EditDimension("Gage", "sk:L", Default.AngleBrace_Gage);
            EditDimension("THK", "sk:L", Default.AngleBrace_THK);
            EditDimension("K", "sk:L", Default.AngleBrace_K);
        }


        // Property overrides
        public override string SizeOrThickness => (Default.AngleBrace_Leg1 >= Default.AngleBrace_Leg2) ? $"{Default.AngleBrace_Leg1}x{Default.AngleBrace_Leg2}x{Default.AngleBrace_THK}" : $"{Default.AngleBrace_Leg2}x{Default.AngleBrace_Leg1}x{Default.AngleBrace_THK}";
        public override Shape RawMaterialShape => Shape.Angle;
    }
}
