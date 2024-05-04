using FileTools.Base;
using ModelTools;
using SolidWorks.Interop.sldworks;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;
using static FileTools.Properties.Settings;

namespace Structure.Columns.Derived.Children
{
    internal class BasePlate : Part
    {
        // Constructor
        public BasePlate(SubAssembly parentAssembly) : base(parentAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Width", "sk:Plate", Default.BasePlate_Width);
            EditDimension("Length", "sk:Plate", Default.BasePlate_Length);
            EditDimension("THK", "Plate", BasePlate_THK);

            EditDimension("Diameter", "sk:Hole", Default.BasePlate_HoleDiameter);
            EditDimension("WidthSpacing", "sk:Hole", Default.BasePlate_WidthHoleSpacing);
            EditDimension("LengthSpacing", "sk:Hole", Default.BasePlate_LengthHoleSpacing);
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
