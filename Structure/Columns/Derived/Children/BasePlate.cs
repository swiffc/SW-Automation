using FileTools.Base;
using ModelTools;
using SolidWorks.Interop.sldworks;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;

namespace Structure.Columns.Derived.Children
{
    internal class BasePlate : Part
    {
        // Static properties
        internal static double BasePlate_Width { get; set; } = 8;
        internal static double BasePlate_Length { get; set; } = 8;
        internal static double BasePlate_WidthHoleSpacing { get; set; } = 3.5;
        internal static double BasePlate_LengthHoleSpacing { get; set; } = 3.5;
        internal static double BasePlate_HoleDiameter { get; set; } = 0.8125;


        // Constructor
        public BasePlate(SubAssembly parentAssembly) : base(parentAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Width", "sk:Plate", BasePlate_Width);
            EditDimension("Length", "sk:Plate", BasePlate_Length);
            EditDimension("THK", "Plate", BasePlate_THK);

            EditDimension("Diameter", "sk:Hole", BasePlate_HoleDiameter);
            EditDimension("WidthSpacing", "sk:Hole", BasePlate_WidthHoleSpacing);
            EditDimension("LengthSpacing", "sk:Hole", BasePlate_LengthHoleSpacing);
        }


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => "102";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => BasePlate_THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                return new List<PositionData>
                {
                    PositionData.Create()
                };
            }
        }


    }
}
