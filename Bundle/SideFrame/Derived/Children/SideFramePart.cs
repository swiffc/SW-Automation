using FileTools.Base;
using ModelTools;
using static FileTools.CommonData.CommonData;

namespace Bundle.SideFrame.Derived.Children
{
    internal abstract class SideFramePart : Part
    {
        // Static properties
        public static double THK
        {
            get { return SideFrame_THK; }
            set { SideFrame_THK = value; }
        }
        public static double Depth
        {
            get { return SideFrame_Depth; }
            set { SideFrame_Depth = value; }
        }
        static public double Flange => 3;


        // Constructor
        protected SideFramePart(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("THK", "SheetMetal", THK);
            EditDimension("R", "SheetMetal", BendTable.GetBendRadius(THK));

            EditDimension("Depth", "sk:Plate", Depth);
            EditDimension("Flange", "sk:Plate", Depth + (IsToedOut ? - Flange : + Flange));


        }


        // Property overrides
        public override bool Enabled => true;
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
    }
}
