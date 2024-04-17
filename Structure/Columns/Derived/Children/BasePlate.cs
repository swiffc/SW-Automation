using FileTools.Base;
using ModelTools;
using SolidWorks.Interop.sldworks;
using System.Collections.Generic;

namespace Structure.Columns.Derived.Children
{
    internal class BasePlate : Part
    {
        // Static properties
        internal static double THK { get; set; } = 0.5;
        internal static double LocalWidth { get; set; } = 12;
        internal static double LocalLength { get; set; } = 8;
        internal static double WidthHoleSpacing { get; set; } = 5.5;
        internal static double LengthHoleSpacing { get; set; } = 3.5;
        internal static double HoleDiameter { get; set; } = 0.8125;


        // Constructor
        public BasePlate(SubAssembly parentAssembly) : base(parentAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Width", "sk:Plate", LocalWidth);
            EditDimension("Length", "sk:Plate", LocalLength);
            EditDimension("THK", "Plate", THK);

            EditDimension("Diameter", "sk:Hole", HoleDiameter);
            EditDimension("WidthSpacing", "sk:Hole", WidthHoleSpacing);
            EditDimension("LengthSpacing", "sk:Hole", LengthHoleSpacing);
        }


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => "102";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
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
