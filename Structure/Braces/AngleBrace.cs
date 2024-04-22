using FileTools.Base;
using Structure.Braces.Derived;
using Structure.Columns.Derived.Children;
using static FileTools.FileTools;
using static FileTools.CommonData.CommonData;

namespace Structure.Braces
{
    internal abstract class AngleBrace : Part
    {
        // Static properties
        static public double Leg1 { get; set; } = 3;
        static public double Leg2 { get; set; } = 3;
        static public double Gage { get; set; } = 1.75;
        static public double THK { get; set; } = 0.25;
        static public double K { get; set; } = 0.5625;


        // Constructor
        protected AngleBrace(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "L", (BraceType == "L" || BraceType == "LL") ? BraceL.LocalLength : BraceX.LocalLength);
            EditDimension("Diameter", "sk:Hole", HoleDiameter_Structural);
            EditDimension("Leg1", "sk:L", Leg1);
            EditDimension("Leg2", "sk:L", Leg2);
            EditDimension("Gage", "sk:L", Gage);
            EditDimension("THK", "sk:L", THK);
            EditDimension("K", "sk:L", K);
        }


        // Property overrides
        public override string SizeOrThickness => (Leg1 >= Leg2) ? $"{Leg1}x{Leg2}x{THK}" : $"{Leg2}x{Leg1}x{THK}";
        public override Shape RawMaterialShape => Shape.Angle;
    }
}
